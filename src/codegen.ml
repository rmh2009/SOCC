open Tokens
open Util
open Type
open Typeutil
open Codegen_util
module VarMap = Map.Make (String)

exception CodeGenError of string

let fail msg = raise (CodeGenError msg)

let default_var_map =
  {
    vars = VarMap.empty;
    cur_scope_vars = VarMap.empty;
    type_defs = VarMap.empty;
    (* Start the offset as 0, since %ebp = %esp. *)
    index = 0;
    break_label = "";
    continue_label = "";
    function_return_label = "";
    static_data = [];
  }

type context_t = {
  output : string -> unit;
  get_unique_label : string -> string;
  (* Used to store the maximum (minimum since it's negative) index. We will
   * use this to allocate stack at once at the beginning of a function.
   * Temporary variables may also be allocated here so that we don't have to
   * call push and pop. *)
  set_min_index : int -> unit;
  get_min_index : unit -> int;
}

module MakeCodeGen (CG : CodeGenUtil_t) = struct
  (*  Helper to 'virtually' allocate a space on stack, this assumes that we are
   *  substracting number of bytes from %esp/%rsp register, and return the offset
   *  from %ebp, as well as the string form 'offset(%ebp), and the new var_map.
   *  This takes care of tracking the maximum stack depth, so that it could be
   *  allocated at once. It also takes care of alignments.' *)
  let allocate_stack (ctx : context_t) (vars : var_map_t) (bytes : int) :
      int * string * var_map_t =
    let align = if bytes < 8 then bytes else 8 in
    let r = -vars.index mod align in
    let new_index =
      if r = 0 then vars.index - bytes else vars.index - align + r - bytes
    in
    if ctx.get_min_index () > new_index then ctx.set_min_index new_index else ();
    (new_index, "", { vars with index = new_index })

  let get_function_call_padding num_args : int =
    let res =
      if CG.is_32_bit then num_args * 4 mod 16
      else
        let args_on_stack = if num_args > 6 then num_args - 6 else 0 in
        args_on_stack * 8 mod 16
    in
    if res = 0 then 0 else 16 - res

  let add_function_call_padding (ctx : context_t) (num_args : int) : unit =
    let padding = get_function_call_padding num_args in
    if padding = 0 then ()
    else (
      ctx.output "#---------- Add function call align -----\n";
      ctx.output
        (CG.gen_command
           (Sub (Imm (get_function_call_padding num_args), Reg SP))
           (PointerType VoidType)) )

  let remove_function_call_padding (ctx : context_t) num_args : unit =
    let padding = get_function_call_padding num_args in
    if CG.is_32_bit then
      if padding = 0 && num_args = 0 then ()
      else (
        ctx.output "#---------- remove function call align -----\n";
        ctx.output
          (Printf.sprintf "    addl    $%d,%%esp\n" (padding + (4 * num_args)))
        )
    else
      let args_on_stack = if num_args > 6 then num_args - 6 else 0 in
      if padding = 0 && args_on_stack = 0 then ()
      else (
        ctx.output "#---------- remove function call align -----\n";
        ctx.output
          (Printf.sprintf "    addq    $%d,%%rsp\n"
             (padding + (8 * args_on_stack))) )

  (* Generate code for calling a function. This includes adding padding for
   * alignment, pushing parameters, remove padding afterwards, etc.*)
  let rec generate_f_call ctx var_map fname exps =
    add_function_call_padding ctx (List.length exps);
    let exps_reversed = List.rev exps in
    let rec helper var_map fname exps num_args =
      match exps with
      | [] ->
          ctx.output ("    call    _" ^ fname ^ "\n");
          (* Remove the padding *)
          remove_function_call_padding ctx num_args
      | a :: r ->
          let t = generate_expression ctx var_map a in
          ignore t;
          ctx.output "    push   %eax\n";
          helper var_map fname r num_args
    in
    helper var_map fname exps_reversed (List.length exps)

  (* Generate code for calling a function for 64 assmebly. This includes adding
   * padding for alignment, pushing parameters, remove padding afterwards, etc.*)
  and generate_f_call_64 ctx var_map fname exps =
    if List.length exps > 6 then
      fail "Does not support more than 6 params in 64 bit mode yet.";
    add_function_call_padding ctx (List.length exps);
    (* registers is the available registers that can be used to store the function
       * arguments. This feature is a new feature in the x86-64 calling convention. *)
    let rec helper var_map fname exps registers num_args =
      match exps with
      | [] ->
          ctx.output ("    call    _" ^ fname ^ "\n");
          (* Remove the padding *)
          remove_function_call_padding ctx num_args
      | a :: r -> (
          let t = generate_expression ctx var_map a in
          ignore t;
          match registers with
          | hd :: tl ->
              ctx.output ("    movq    %rax, " ^ hd ^ "\n");
              helper var_map fname r tl num_args
          | [] ->
              ctx.output "    pushq    %rax\n";
              helper var_map fname r [] num_args )
    in
    helper var_map fname exps CG.fun_arg_registers_64 (List.length exps)

  (* Generates the assembly code for a specific expression. *)
  and generate_expression (ctx : context_t) (var_map : var_map_t)
      (exp : expression_t) : data_type_t =
    let output = ctx.output in
    let gen_command a b = CG.gen_command a b |> output in
    let get_unique_label = ctx.get_unique_label in
    let pvoid = PointerType VoidType in
    let generate_relational_expression output command exp1 exp2 : data_type_t =
      generate_expression ctx var_map exp1 |> ignore;
      let off, _, var_map = allocate_stack ctx var_map 4 in
      gen_command (Mov (Reg AX, Disp (off, BP))) IntType;
      generate_expression ctx var_map exp2 |> ignore;
      gen_command (Mov (Disp (off, BP), Reg CX)) IntType;
      gen_command (Comp (Reg AX, Reg CX)) IntType;
      gen_command (Mov (Imm 0, Reg AX)) IntType;
      output (command ^ "   %al\n");
      IntType
    in
    match exp with
    | VarExp a -> (
        match VarMap.find_opt a var_map.vars with
        | None -> fail ("Variable " ^ a ^ " is undefined.")
        | Some (offset, t, _) ->
            ( match t with
            | IntType -> gen_command (Mov (Disp (offset, BP), Reg AX)) IntType
            | ArrayType (_, _) ->
                gen_command (Lea (Disp (offset, BP), Reg AX)) pvoid
            | StructType (_, _) ->
                gen_command (Lea (Disp (offset, BP), Reg AX)) pvoid
            | PointerType _ ->
                gen_command (Mov (Disp (offset, BP), Reg AX)) pvoid
            | CharType ->
                gen_command (Xor (Reg AX, Reg AX)) pvoid;
                gen_command (Mov (Disp (offset, BP), Reg AX)) CharType
            | x -> fail ("Unsupported type in VarExp." ^ Debug.print_data_type x)
            );
            t )
    | PreIncExp exp -> (
        match exp with
        | VarExp a -> (
            match VarMap.find_opt a var_map.vars with
            | None -> fail ("Variable " ^ a ^ " is undefined.")
            | Some (offset, t, _) ->
                gen_command (Inc (Disp (offset, BP))) t;
                gen_command (Mov (Disp (offset, BP), Reg AX)) t;
                t )
        | ArrayIndexExp (exp1, exp2) ->
            fail "PreInc of ArrayIndex not implemented yet."
        | _ -> fail "PreIncExp expecting VarExp or ArrayIndexExp only." )
    | PostIncExp exp -> (
        match exp with
        | VarExp a -> (
            match VarMap.find_opt a var_map.vars with
            | None -> fail ("Variable " ^ a ^ " is undefined.")
            | Some (offset, t, _) ->
                gen_command (Mov (Disp (offset, BP), Reg AX)) t;
                gen_command (Inc (Disp (offset, BP))) t;
                t )
        | ArrayIndexExp (exp1, exp2) ->
            fail "PostInc of ArrayIndex not implemented yet."
        | _ -> fail "PostIncExp expecting VarExp or ArrayIndexExp only." )
    | PreDecExp exp -> (
        match exp with
        | VarExp a -> (
            match VarMap.find_opt a var_map.vars with
            | None -> fail ("Variable " ^ a ^ " is undefined.")
            | Some (offset, t, _) ->
                gen_command (Dec (Disp (offset, BP))) t;
                gen_command (Mov (Disp (offset, BP), Reg AX)) t;
                t )
        | ArrayIndexExp (exp1, exp2) ->
            fail "PreDec of ArrayIndex not implemented yet."
        | _ -> fail "PreDecExp expecting VarExp or ArrayIndexExp only." )
    | PostDecExp exp -> (
        match exp with
        | VarExp a -> (
            match VarMap.find_opt a var_map.vars with
            | None -> fail ("Variable " ^ a ^ " is undefined.")
            | Some (offset, t, _) ->
                gen_command (Mov (Disp (offset, BP), Reg AX)) t;
                gen_command (Dec (Disp (offset, BP))) t;
                t )
        | ArrayIndexExp (exp1, exp2) ->
            fail "PostDec of ArrayIndex not implemented yet."
        | _ -> fail "PostDecExp expecting VarExp or ArrayIndexExp only." )
    | ArrayIndexExp (exp1, exp2) -> (
        (* exp1 must be of type Array, in the current implementation types are evaluated
         * during code generation, the evaluation result and step size also depends on
         * the type of exp1, so we need to evaluate exp1 first.*)
        let t = generate_expression ctx var_map exp1 in
        match t with
        | ArrayType (child_type, _) | PointerType (ArrayType (child_type, _)) ->
            let off, _, var_map =
              allocate_stack ctx var_map (CG.get_data_size var_map pvoid)
            in
            gen_command (Mov (Reg AX, Disp (off, BP))) pvoid;
            generate_expression ctx var_map exp2 |> ignore;
            gen_command (Mov (Disp (off, BP), Reg CX)) pvoid;
            gen_command
              (Mul (Imm (CG.get_data_size var_map child_type), Reg AX))
              pvoid;
            gen_command (Add (Reg CX, Reg AX)) pvoid;
            gen_command (Mov (Reg AX, Reg DX)) pvoid;
            (* gen_command
               (Lea (Index (0, CX, AX, CG.get_data_size var_map child_type), Reg DX))
               pvoid; *)
            if not (should_array_element_be_address child_type) then (
              gen_command (Xor (Reg AX, Reg AX)) pvoid;
              gen_command (Mov (RegV DX, Reg AX)) child_type )
            else gen_command (Mov (Reg DX, Reg AX)) pvoid;
            child_type
        | _ ->
            fail
              ( "Target is not array type: "
              ^ Debug.print_expression 0 exp1
              ^ ", actual type: " ^ Debug.print_data_type t ) )
    | StructMemberExp (exp, member) | ArrowStructMemberExp (exp, member) -> (
        let t = generate_expression ctx var_map exp in
        match t with
        | StructType (name, _) | PointerType (StructType (name, _)) -> (
            let struct_info = VarMap.find name var_map.type_defs in
            let _, dtype, offset = VarMap.find member struct_info.members in
            match dtype with
            | ArrayType (_, _) | StructType (_, _) ->
                gen_command (Lea (Disp (offset, AX), Reg AX)) pvoid;
                dtype
            | _ ->
                gen_command (Mov (Disp (offset, AX), Reg AX)) dtype;
                dtype )
        | _ ->
            "Expecting struct type in StructMemberExp or ArrowStructMemberExp, \
             actual type: " ^ Debug.print_data_type t
            |> fail )
    | DereferenceExp exp -> (
        let t = generate_expression ctx var_map exp in
        if not (is_type_pointer t) then
          fail "Only pointer type can be dereferenced."
        else gen_command (Mov (RegV AX, Reg AX)) t;
        match t with
        | PointerType inner -> inner
        | _ -> fail "Expecting a pointer type in DereferenceExp." )
    | AddressOfExp (VarExp a) -> (
        match VarMap.find_opt a var_map.vars with
        | None -> fail ("Variable " ^ a ^ " is undefined.")
        | Some (offset, t, _) -> (
            match t with
            | IntType | ArrayType (_, _) | PointerType _ | StructType (_, _) ->
                gen_command (Lea (Disp (offset, BP), Reg AX)) pvoid;
                PointerType t
            | _ -> fail "Illegal type in AddressOfExp." ) )
    | AddressOfExp (ArrayIndexExp (exp1, exp2)) -> (
        (* TODO remove this duplicate code with the ArrayIndexExp code above. *)
        let t = generate_expression ctx var_map exp1 in
        match t with
        | ArrayType (child_type, size) ->
            let off, _, var_map =
              allocate_stack ctx var_map (CG.get_data_size var_map pvoid)
            in
            gen_command (Mov (Reg AX, Disp (off, BP))) pvoid;
            generate_expression ctx var_map exp2 |> ignore;
            gen_command (Mov (Disp (off, BP), Reg CX)) pvoid;
            gen_command
              (Mul (Imm (CG.get_data_size var_map child_type), Reg AX))
              pvoid;
            gen_command (Add (Reg CX, Reg AX)) pvoid;
            (* gen_command
               (Lea (Index (0, CX, AX, CG.get_data_size var_map child_type), Reg AX))
               pvoid; *)
            PointerType child_type
        | _ -> fail "Expecting an array type in ArrayIndexExp." )
    | AddressOfExp _ ->
        fail "Can only take address of array element or varaible."
    | ConstantIntExp n ->
        gen_command (Mov (Imm n, Reg AX)) IntType;
        IntType
    | ConstantCharExp a ->
        gen_command (Xor (Reg AX, Reg AX)) pvoid;
        gen_command (Mov (Imm (int_of_char a), Reg AX)) CharType;
        CharType
    | ConstantStringExp a -> fail "String unimplemented"
    | ConstantFloatExp a -> fail "Float unimplemented"
    | FunctionCallExp (fname, exps) ->
        if CG.is_32_bit then generate_f_call ctx var_map fname exps
        else generate_f_call_64 ctx var_map fname exps;
        (* TODO assume function always return int. *)
        IntType
    | NegateOp exp ->
        let t = generate_expression ctx var_map exp in
        gen_command (Neg (Reg AX)) t;
        t
    | LogicalNegateOp exp ->
        let t = generate_expression ctx var_map exp in
        gen_command (Comp (Imm 0, Reg AX)) t;
        gen_command (Mov (Imm 0, Reg AX)) IntType;
        (* zero %ax completely. *)
        output "    sete   %al\n";
        t
    | ComplementOp exp ->
        let t = generate_expression ctx var_map exp in
        gen_command (Not (Reg AX)) t;
        t
    | GroupedExpression exp -> generate_expression ctx var_map exp
    | AdditionExp (exp1, exp2) ->
        let t1 = generate_expression ctx var_map exp1 in
        let off, _, var_map =
          allocate_stack ctx var_map (CG.get_data_size var_map t1)
        in
        gen_command (Mov (Reg AX, Disp (off, BP))) t1;
        let t2 = generate_expression ctx var_map exp2 in
        gen_command (Mov (Disp (off, BP), Reg CX)) t1;
        let larger_t =
          if CG.get_data_size var_map t1 > CG.get_data_size var_map t2 then t1
          else t2
        in
        gen_command (Add (Reg CX, Reg AX)) larger_t;
        t1
    | MinusExp (exp1, exp2) ->
        let t2 = generate_expression ctx var_map exp2 in
        let off, _, var_map =
          allocate_stack ctx var_map (CG.get_data_size var_map t2)
        in
        gen_command (Mov (Reg AX, Disp (off, BP))) t2;
        let t1 = generate_expression ctx var_map exp1 in
        gen_command (Mov (Disp (off, BP), Reg CX)) t1;
        let larger_t =
          if CG.get_data_size var_map t1 > CG.get_data_size var_map t2 then t1
          else t2
        in
        gen_command (Sub (Reg CX, Reg AX)) larger_t;
        t1
    | MultiExp (exp1, exp2) ->
        let t1 = generate_expression ctx var_map exp1 in
        let off, _, var_map =
          allocate_stack ctx var_map (CG.get_data_size var_map t1)
        in
        gen_command (Mov (Reg AX, Disp (off, BP))) t1;
        let t2 = generate_expression ctx var_map exp2 in
        gen_command (Mov (Disp (off, BP), Reg CX)) t1;
        if CG.get_data_size var_map t1 != CG.get_data_size var_map t2 then
          fail
            ( "t1 and t2 not equal in binary operation: "
            ^ Debug.print_data_type t1 ^ ", " ^ Debug.print_data_type t2 );
        gen_command (Mul (Reg CX, Reg AX)) t1;
        t1
    | DivideExp (exp1, exp2) ->
        let t2 = generate_expression ctx var_map exp2 in
        let off, _, var_map =
          allocate_stack ctx var_map (CG.get_data_size var_map t2)
        in
        gen_command (Mov (Reg AX, Disp (off, BP))) t2;
        let t1 = generate_expression ctx var_map exp1 in
        if CG.get_data_size var_map t1 != CG.get_data_size var_map t2 then
          fail
            ( "t1 and t2 not equal in binary operation: "
            ^ Debug.print_data_type t1 ^ ", " ^ Debug.print_data_type t2 );

        (* need to sign extend eax to 64 bit edx:eax if t1 is 32bit, or sign extend rax to 128 bit rdx:rax if 64bit *)
        let dsize = CG.get_data_size var_map t1 in
        if dsize = 4 then output "cdq\n"
        else if dsize = 8 then output "cqto"
        else fail "Division of unsupported data type.";

        gen_command (Mov (Disp (off, BP), Reg CX)) t1;
        gen_command (Div (Reg CX)) t1;
        t1
    | EqualExp (exp1, exp2) ->
        generate_relational_expression output "    sete" exp1 exp2
    | NotEqualExp (exp1, exp2) ->
        generate_relational_expression output "    setne" exp1 exp2
    | GreaterOrEqualExp (exp1, exp2) ->
        generate_relational_expression output "    setge" exp1 exp2
    | GreaterExp (exp1, exp2) ->
        generate_relational_expression output "    setg" exp1 exp2
    | LessOrEqualExp (exp1, exp2) ->
        generate_relational_expression output "    setle" exp1 exp2
    | LessExp (exp1, exp2) ->
        generate_relational_expression output "    setl" exp1 exp2
    | OrExp (exp1, exp2) ->
        let t1 = generate_expression ctx var_map exp1 in
        let clause_label = get_unique_label "_clause2" in
        let end_label = get_unique_label "_end" in
        gen_command (Comp (Imm 0, Reg AX)) t1;
        gen_command (Je clause_label) IntType;
        gen_command (Mov (Imm 1, Reg AX)) IntType;
        (* always output int *)
        gen_command (Jmp end_label) IntType;
        output (clause_label ^ ":\n");
        let t2 = generate_expression ctx var_map exp2 in
        gen_command (Comp (Imm 0, Reg AX)) t2;
        gen_command (Mov (Imm 0, Reg AX)) IntType;
        (* always output int *)
        output "    setne    %al\n";
        output (end_label ^ ":\n");
        IntType
    | AndExp (exp1, exp2) ->
        let t1 = generate_expression ctx var_map exp1 in
        let clause_label = get_unique_label "_clause2" in
        let end_label = get_unique_label "_end" in
        gen_command (Comp (Imm 0, Reg AX)) t1;
        gen_command (Jne clause_label) IntType;
        gen_command (Mov (Imm 0, Reg AX)) IntType;
        (* always output int *)
        gen_command (Jmp end_label) IntType;
        output (clause_label ^ ":\n");
        let t2 = generate_expression ctx var_map exp2 in
        gen_command (Comp (Imm 0, Reg AX)) t2;
        gen_command (Mov (Imm 0, Reg AX)) IntType;
        (* always output int *)
        output "    setne    %al\n";
        output (end_label ^ ":\n");
        IntType
    | ConditionExp (exp1, exp2, exp3) ->
        let t1 = generate_expression ctx var_map exp1 in
        let cond_label = get_unique_label "_cond" in
        let cond_end_label = get_unique_label "_condend" in
        gen_command (Comp (Imm 0, Reg AX)) t1;
        gen_command (Je cond_label) IntType;
        let t2 = generate_expression ctx var_map exp2 in
        gen_command (Jmp cond_end_label) IntType;
        output (cond_label ^ ":\n");
        let t3 = generate_expression ctx var_map exp3 in
        output (cond_end_label ^ ":\n");
        if CG.get_data_size var_map t2 != CG.get_data_size var_map t3 then
          fail
            ( "t1 and t2 not equal in binary operation: "
            ^ Debug.print_data_type t2 ^ ", " ^ Debug.print_data_type t3 );
        t2
    | AssignExp (VarExp a, exp) -> (
        match VarMap.find_opt a var_map.vars with
        | None -> fail ("Variable " ^ a ^ " is undefined.")
        | Some (offset, t, _) ->
            let t = generate_expression ctx var_map exp in
            gen_command (Mov (Reg AX, Disp (offset, BP))) t;
            t )
    | AssignExp (ArrayIndexExp (exp1, exp2), exp_r) -> (
        (* TODO Remove ruplicate code with the other ArrayIndexExp code. *)
        let t = generate_expression ctx var_map exp1 in
        let offset, _, var_map =
          allocate_stack ctx var_map (CG.get_data_size var_map pvoid)
        in
        gen_command (Mov (Reg AX, Disp (offset, BP))) pvoid;
        output "# array assign addr above\n";
        match t with
        | ArrayType (element_type, size)
        | PointerType (ArrayType (element_type, size)) ->
            if is_type_array element_type then
              fail "Only 1-D array is assignable."
            else
              let t_index = generate_expression ctx var_map exp2 in
              gen_command (Mov (Disp (offset, BP), Reg CX)) pvoid;
              gen_command
                (Mul (Imm (CG.get_data_size var_map element_type), Reg AX))
                pvoid;
              gen_command (Add (Reg CX, Reg AX)) pvoid;
              (* gen_command
                 (Lea (Index (0, CX, AX, CG.get_data_size var_map element_type), Reg AX))
                 pvoid; *)
              (* Reuse the temp_loc here, since it's already popped. *)
              gen_command (Mov (Reg AX, Disp (offset, BP))) pvoid;
              let t_value = generate_expression ctx var_map exp_r in
              (* get the address to assign to *)
              gen_command (Mov (Disp (offset, BP), Reg CX)) pvoid;
              gen_command (Mov (Reg AX, RegV CX)) element_type;
              ignore t_index;
              ignore t_value;
              element_type
        | a -> fail ("Type is not assignable: " ^ Debug.print_data_type a) )
    | AssignExp (DereferenceExp exp, exp_r) ->
        let t = generate_expression ctx var_map exp in
        let offset, _, var_map =
          allocate_stack ctx var_map (CG.get_data_size var_map pvoid)
        in
        gen_command (Mov (Reg AX, Disp (offset, BP))) pvoid;
        let t2 = generate_expression ctx var_map exp_r in
        ( match t with
        | PointerType pt ->
            if pt != t2 then
              fail
                ( "Type not the same in Assignment expression!"
                ^ Debug.print_data_type t ^ " vs. " ^ Debug.print_data_type t2
                )
            else ()
        | a -> fail ("Type can not be dereferenced! " ^ Debug.print_data_type a)
        );
        gen_command (Mov (Disp (offset, BP), Reg CX)) pvoid;
        gen_command (Mov (Reg AX, RegV CX)) t2;
        t2
    | AssignExp (StructMemberExp (exp, member), exp_r)
    | AssignExp (ArrowStructMemberExp (exp, member), exp_r) -> (
        let t = generate_expression ctx var_map exp in
        let offset, _, var_map =
          allocate_stack ctx var_map (CG.get_data_size var_map pvoid)
        in
        match t with
        | StructType (name, _) | PointerType (StructType (name, _)) -> (
            let struct_info = VarMap.find name var_map.type_defs in
            let _, dtype, member_offset =
              VarMap.find member struct_info.members
            in
            match dtype with
            | ArrayType (_, _) | StructType (_, _) ->
                "Struct member is not assignable!" ^ Debug.print_data_type dtype
                |> fail
            | _ ->
                gen_command (Lea (Disp (member_offset, AX), Reg AX)) pvoid;
                gen_command (Mov (Reg AX, Disp (offset, BP))) pvoid;
                let t_value = generate_expression ctx var_map exp_r in
                (* get the address to assign to *)
                gen_command (Mov (Disp (offset, BP), Reg CX)) pvoid;
                gen_command (Mov (Reg AX, RegV CX)) t_value;
                t_value )
        | _ ->
            "Expecting struct type in StructMemberExp, actual type: "
            ^ Debug.print_data_type t
            |> fail )
    | AssignExp (_, _) -> fail "Left hand side is not assignable!"

  (* Updates the continue_label and break_label fields in the given var_map. *)
  let update_break_continue_label (var_map : var_map_t) (break : string)
      (continue : string) : var_map_t =
    { var_map with continue_label = continue; break_label = break }

  (* Generates assmebly code for a block item. *)
  let rec generate_block_item (ctx : context_t) (var_map : var_map_t)
      (st : block_item_t) : var_map_t =
    let output = ctx.output in
    let gen_command a b = CG.gen_command a b |> output in
    let get_unique_label = ctx.get_unique_label in
    match st with
    | StatementItem (ReturnStatement exp) ->
        generate_expression ctx var_map exp |> ignore;
        gen_command (Jmp var_map.function_return_label) IntType;
        var_map
    | StatementItem (ExpressionStatement (Some exp)) ->
        generate_expression ctx var_map exp |> ignore;
        var_map
    | StatementItem (ExpressionStatement None) -> var_map
    | StatementItem (ConditionalStatement (exp, st1, Some st2)) ->
        let cond_label = get_unique_label "_cond" in
        let cond_end_label = get_unique_label "_condend" in
        let t = generate_expression ctx var_map exp in
        gen_command (Comp (Imm 0, Reg AX)) t;
        gen_command (Je cond_label) IntType;
        let var_map = generate_block_item ctx var_map (StatementItem st1) in
        gen_command (Jmp cond_end_label) IntType;
        output (cond_label ^ ":\n");
        generate_block_item ctx var_map (StatementItem st2) |> ignore;
        output (cond_end_label ^ ":\n");
        var_map
    | StatementItem (ConditionalStatement (exp, st1, None)) ->
        let cond_end_label = get_unique_label "_condend" in
        let t = generate_expression ctx var_map exp in
        gen_command (Comp (Imm 0, Reg AX)) t;
        gen_command (Je cond_end_label) IntType;
        let var_map = generate_block_item ctx var_map (StatementItem st1) in
        output (cond_end_label ^ ":\n");
        var_map
    | StatementItem (ForStatement (exp1_opt, exp2, exp3_opt, st)) ->
        let cond_label = get_unique_label "_forcond" in
        let end_label = get_unique_label "_forend" in
        let break_label = end_label in
        let continue_label = get_unique_label "_forcontinue" in
        ( match exp1_opt with
        | None -> ()
        | Some exp ->
            generate_expression ctx var_map exp |> ignore;
            () );
        output (cond_label ^ ":\n");
        let t = generate_expression ctx var_map exp2 in
        gen_command (Comp (Imm 0, Reg AX)) t;
        gen_command (Je end_label) IntType;
        generate_block_item ctx
          (update_break_continue_label var_map break_label continue_label)
          (StatementItem st)
        |> ignore;
        output (continue_label ^ ":\n");
        ( match exp3_opt with
        | None -> ()
        | Some exp ->
            generate_expression ctx var_map exp |> ignore;
            () );
        gen_command (Jmp cond_label) IntType;
        output (end_label ^ ":\n");
        var_map
    | StatementItem (ForDeclStatement (declare, exp2, exp3_opt, st)) ->
        let cond_label = get_unique_label "_forcond" in
        let end_label = get_unique_label "_forend" in
        let break_label = end_label in
        let continue_label = get_unique_label "_forcontinue" in
        let condition_var_map =
          generate_block_item ctx
            {
              var_map with
              cur_scope_vars = VarMap.empty;
              break_label = "";
              continue_label = "";
            }
            (DeclareItem declare)
        in
        output (cond_label ^ ":\n");
        let t = generate_expression ctx condition_var_map exp2 in
        gen_command (Comp (Imm 0, Reg AX)) t;
        gen_command (Je end_label) IntType;
        generate_block_item ctx
          (update_break_continue_label condition_var_map break_label
             continue_label)
          (StatementItem st)
        |> ignore;
        output (continue_label ^ ":\n");
        ( match exp3_opt with
        | None -> ()
        | Some exp -> generate_expression ctx condition_var_map exp |> ignore );
        gen_command (Jmp cond_label) IntType;
        output (end_label ^ ":\n");
        var_map
    | StatementItem BreakStatement ->
        if var_map.break_label = "" then fail "Illegal break, no context.";
        gen_command (Jmp var_map.break_label) IntType;
        var_map
    | StatementItem ContinueStatement ->
        if var_map.continue_label = "" then fail "Illegal jump, no context.";
        gen_command (Jmp var_map.continue_label) IntType;
        var_map
    | StatementItem (WhileStatement (exp, st)) ->
        let cond_label = get_unique_label "_whilecond" in
        let end_label = get_unique_label "_whileend" in
        let break_label = end_label in
        let continue_label = cond_label in
        output (cond_label ^ ":\n");
        let t = generate_expression ctx var_map exp in
        gen_command (Comp (Imm 0, Reg AX)) t;
        gen_command (Je end_label) IntType;
        generate_block_item ctx
          (update_break_continue_label var_map break_label continue_label)
          (StatementItem st)
        |> ignore;
        gen_command (Jmp cond_label) IntType;
        output (end_label ^ ":\n");
        var_map
    | StatementItem (DoStatement (st, exp)) ->
        let begin_label = get_unique_label "_dobegin" in
        let end_label = get_unique_label "_doend" in
        let break_label = end_label in
        let continue_label = get_unique_label "_docontinue" in
        output (begin_label ^ ":\n");
        generate_block_item ctx
          (update_break_continue_label var_map break_label continue_label)
          (StatementItem st)
        |> ignore;
        output (continue_label ^ ":\n");
        let t = generate_expression ctx var_map exp in
        gen_command (Comp (Imm 0, Reg AX)) t;
        gen_command (Jne begin_label) IntType;
        output (end_label ^ ":\n");
        var_map
    | StatementItem (CompoundStatement items) ->
        (* Entering a new scope, so clear the cur_scope_vars, but we ignore the inner var_map returned. *)
        generate_block_statements ctx
          { var_map with cur_scope_vars = VarMap.empty }
          items
        |> ignore;
        var_map
    | DeclareItem (DeclareStatement (data_type, a, exp_opt)) -> (
        (* TODO add type check here for data_type and the type returned by exp_opt *)
        (* data_opt represents any static data that may have been defined in exp *)
        let data_opt =
          match exp_opt with
          | None ->
              gen_command (Mov (Imm 0, Reg AX)) IntType |> ignore;
              None
          | Some (ConstantStringExp str) ->
              Some (ctx.get_unique_label "_data", DataString str)
          | Some exp ->
              generate_expression ctx var_map exp |> ignore;
              None
        in

        (* allocate the data block on stack. *)
        let start_offset, _, var_map =
          allocate_stack ctx var_map (CG.get_data_size var_map data_type)
        in

        ( match (data_type, data_opt) with
        (* String initialization. *)
        | ArrayType (_, _), Some (label, DataString str) ->
            (* Use memcpy to copy the static string to targe. *)
            output
              (Printf.sprintf
                 "# --------- memcpy %s %s to array variable: %s -------\n"
                 label str a);
            gen_command
              (Lea (Disp (start_offset, BP), Reg AX))
              (PointerType VoidType);
            CG.call_memcpy AX label (String.length str) |> output
        | _, _ ->
            gen_command (Mov (Reg AX, Disp (start_offset, BP))) data_type
            |> ignore );

        match VarMap.find_opt a var_map.cur_scope_vars with
        | Some (_, _, _) ->
            fail ("Var " ^ a ^ " is already defined in current scope!")
        | None ->
            {
              var_map with
              vars = VarMap.add a (start_offset, data_type, None) var_map.vars;
              cur_scope_vars =
                VarMap.add a
                  (start_offset, data_type, None)
                  var_map.cur_scope_vars;
              index = start_offset;
              static_data =
                ( match data_opt with
                | None -> var_map.static_data
                | Some data -> data :: var_map.static_data );
            } )

  (* Generates the assembly code for a list of block items. *)
  and generate_block_statements (ctx : context_t) (var_map : var_map_t)
      (sts : block_item_t list) : var_map_t =
    List.fold_left (generate_block_item ctx) var_map sts

  let generate_global (ctx : context_t) (var_map : var_map_t)
      (g : global_item_t) : var_map_t =
    (* Generates the var_map with the references to the function arguments. *)
    let rec generate_f_var_map ctx var_map fname
        (params : (string * data_type_t) list) index : var_map_t =
      match params with
      | [] -> var_map
      | (name, dtype) :: r ->
          generate_f_var_map ctx
            {
              var_map with
              vars = VarMap.add name (index, decay dtype, None) var_map.vars;
              break_label = "";
              continue_label = "";
            }
            fname r (index + 4)
    in

    let rec generate_f_var_map_64_bit ctx var_map fname
        (params : (string * data_type_t) list) index : var_map_t =
      (* for 64 bit, we always copy function arguments from registers to stack. *)
      if List.length params > 6 then
        fail "Only support at most 6 function params now in 64 bit mode.";
      let rec gen_fun ctx var_map params regs =
        match (params, regs) with
        | [], _ -> var_map
        | (name, dtype) :: ar, b :: br ->
            let offset, _, var_map = allocate_stack ctx var_map 8 in
            ctx.output
              (Printf.sprintf "    movq    %s, %d(%s) # moving fun param\n" b
                 offset "%rbp");
            gen_fun ctx
              {
                var_map with
                vars = VarMap.add name (offset, decay dtype, None) var_map.vars;
                break_label = "";
                continue_label = "";
              }
              ar br
        | _, _ -> fail "Unexpectd case in generate_f_var_map_64_bit."
      in
      gen_fun ctx var_map params CG.fun_arg_registers_64
    in
    let output = ctx.output in
    let gen_command a b = CG.gen_command a b |> output in

    match g with
    | GlobalFunction (IntFunction (fname, params, items_opt)) -> (
        match items_opt with
        | None -> var_map
        | Some items ->
            output ("\n\n_" ^ fname ^ ":\n");

            (* Function prologue *)
            if not CG.is_64_bit then
              output "    push    %ebp\n    movl    %esp, %ebp\n"
            else output "    push    %rbp\n    movq    %rsp, %rbp\n";

            (* Align main function start so that esp/rsp mod 16 is 8. This makes main consistent
             * with other general functions, so that stack_allocation logic will make every stack
             * aligned correctly. *)
            if fname = "main" then (
              output
                "#---------------- (main only) align esp to 16 bytes --------\n";
              output (CG.align_esp_to_16 ());
              if not CG.is_64_bit then (
                output
                  "#---------- (main only) add extra so that esp % 16 is 8--\n";
                output "    subl    $8, %esp #\n" ) )
            else ();

            (* Reset the index at the function beginning. *)
            ctx.set_min_index 0;
            (* This is a little hacky, we need to generate the output then get the stack size,
             * but we need to output stack allocation command first, so we use a temporary buffer
             * for each function. *)
            let fun_buf = Buffer.create 32 in
            let return_label = ctx.get_unique_label "_fun_return" in
            let ctx_new =
              { ctx with output = (fun a -> Buffer.add_string fun_buf a) }
            in
            let var_map =
              if CG.is_64_bit then
                generate_f_var_map_64_bit ctx_new var_map fname params 8
              else generate_f_var_map ctx_new var_map fname params 8
            in
            let var_map =
              { var_map with function_return_label = return_label }
            in
            (* The block statements will be in the temporary buffer fun_buf. *)
            let res = generate_block_statements ctx_new var_map items in

            (* Stack allocation and function body, always make the stack 16 byte aligned. *)
            output "#---------- stack allocation ----------\n";

            (* This is tricky here, we already have two registered pushed at the beginning of a function,
             * one is the return address saved by call, the other is 'push ebp', so we need to
             * take that into consideration. *)
            let pvoid = PointerType VoidType in
            let two_additional_push = 2 * CG.get_data_size var_map pvoid in
            let total_stack_size =
              -two_additional_push + ctx_new.get_min_index ()
            in
            let stack_allocated =
              make_aligned_number total_stack_size 16 + two_additional_push
            in
            gen_command
              (Add (Imm stack_allocated, Reg SP))
              (PointerType VoidType);
            output "#---------- function body    ----------\n";
            output (Buffer.contents fun_buf);
            (* Return label, for early returns. *)
            output (return_label ^ ":\n");

            output "#---------- stack deallocation --------\n";
            (* Restore esp and ebp. Since we can just use ebp to restore esp, all
             * the special allocations can be handled automatically. *)
            (* Function epilog *)
            if not CG.is_64_bit then
              output "    movl    %ebp, %esp\n    pop    %ebp\n"
            else output "    movq    %rbp, %rsp\n    pop    %rbp\n";
            output "    ret\n";

            { var_map with static_data = res.static_data } )
    | GlobalDef data_type -> (
        match data_type with
        | StructType (a, _) ->
            {
              var_map with
              type_defs =
                VarMap.add a
                  (CG.get_struct_info var_map data_type)
                  var_map.type_defs;
            }
        | a ->
            "Global type definitions can only be struct definitions, saw "
            ^ Debug.print_data_type a
            |> fail )

  (* Generates the assembly code as a string given the ast in Parser.program_t type. *)
  let generate_assembly (ast : program_t) : string =
    let buf = Buffer.create 32 in
    let count = ref 0 in
    (* minimum offset of %esp - %ebp *)
    let min_index = ref 0 in
    let ctx =
      {
        output = Buffer.add_string buf;
        get_unique_label = get_unique_label count;
        set_min_index = (fun a -> min_index := a);
        get_min_index = (fun () -> !min_index);
      }
    in
    ctx.output ".globl _main\n";
    match ast with
    | Program globals ->
        let accumulated_var_map =
          List.fold_left (generate_global ctx) default_var_map globals
        in
        ctx.output (CG.gen_data_section accumulated_var_map.static_data);
        Buffer.contents buf
end
