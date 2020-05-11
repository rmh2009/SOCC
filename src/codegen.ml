open Lexer
open Parser
open Util
open Type
open Typeutil
module VarMap = Map.Make (String)

exception CodeGenError of string

type var_map_t = {
  vars : (int * data_type_t) VarMap.t;
  cur_scope_vars : (int * data_type_t) VarMap.t;
  (* index is the current offset of (%esp - %ebp), this is used to statically associate a new
   * variable to its address, which is offset to frame base %ebp. *)
  index : int;
  break_label : string;
  continue_label : string;
  function_return_label : string;
}

type context_t = {
  output : string -> unit;
  get_unique_label : string -> string;
  is_64_bit : bool;
  (* Used to store the maximum (minimum since it's negative) index. We will
   * use this to allocate stack at once at the beginning of a function.
   * Temporary variables may also be allocated here so that we don't have to
   * call push and pop. *)
  set_min_index : int -> unit;
  get_min_index : unit -> int;
}

(*  Helper to 'virtually' allocate a space on stack, this assumes that we are
 *  substracting number of bytes from %esp/%rsp register, and return the offset
 *  from %ebp, as well as the string form 'offset(%ebp), and the new var_map.
 *  This takes care of tracking the maximum stack depth, so that it could be
 *  allocated at once. It also takes care of alignments.' *)
let allocate_stack (ctx : context_t) (vars : var_map_t) (bytes : int) :
    int * string * var_map_t =
  let r = -vars.index mod bytes in
  let new_index =
    if r = 0 then vars.index - bytes else vars.index - (2 * bytes) + r
  in
  if ctx.get_min_index () > new_index then ctx.set_min_index new_index else ();
  ( new_index,
    string_of_int new_index ^ "(%esp)",
    { vars with index = new_index } )

(* Hacky solution per Nora's article to add padding so that function stack is 16
 * byte aligned. This is for MacOs only. *)
let add_function_call_padding (ctx : context_t) (num_args : int) : unit =
  ctx.output "movl    %esp, %eax\n";
  (* TODO assumed that function param is always int *)
  ctx.output ("subl $" ^ string_of_int (4 * (num_args + 1)) ^ ", %eax\n");
  ctx.output "xorl %edx, %edx\nmovl $0x20, %ecx\nidivl %ecx\n";
  ctx.output "subl %edx, %esp\npushl %edx\n"

let remove_function_call_padding (ctx : context_t) : unit =
  ctx.output "popl %edx\naddl %edx, %esp\n"

(* Injected after certain jump labels to refresh the esp, this is like
 * deallocating objects allocated in the inner scope(s) *)
let generate_update_esp (ctx : context_t) (var_map : var_map_t) : unit =
  (* ctx.output "movl    %ebp, %eax\n";
  ctx.output ("addl    $" ^ string_of_int var_map.index ^ ",  %eax\n");
  ctx.output "movl    %eax, %esp\n" *)
  ()

(* Generate code for calling a function. This includes adding padding for
 * alignment, pushing parameters, remove padding afterwards, etc.*)
let rec generate_f_call ctx var_map fname exps =
  add_function_call_padding ctx (List.length exps);
  let rec helper var_map fname exps num_args =
    match exps with
    | [] ->
        ctx.output ("call    _" ^ fname ^ "\n");
        (* Function returned, remove arguments from stack. *)
        (* TODO assumed that function param is always int *)
        ctx.output ("addl    $" ^ string_of_int (4 * num_args) ^ ", %esp\n");
        (* Remove the padding *)
        remove_function_call_padding ctx
    | a :: r ->
        generate_expression ctx var_map a |> ignore;
        ctx.output "push   %eax\n";
        helper var_map fname r num_args
  in
  helper var_map fname exps (List.length exps)

(* Generates the assembly code for a specific expression. *)
and generate_expression (ctx : context_t) (var_map : var_map_t)
    (exp : expression_t) : data_type_t =
  let output = ctx.output in
  let get_unique_label = ctx.get_unique_label in
  let generate_relational_expression output command exp1 exp2 : data_type_t =
    generate_expression ctx var_map exp1 |> ignore;
    let _, temp_loc, var_map = allocate_stack ctx var_map 4 in
    output ("movl    %eax, " ^ temp_loc ^ "\n");
    generate_expression ctx var_map exp2 |> ignore;
    output ("movl    " ^ temp_loc ^ ", %ecx\n");
    output "cmpl   %eax, %ecx\n";
    output "movl   $0, %eax\n";
    output (command ^ "   %al\n");
    IntType
  in
  match exp with
  | VarExp a -> (
      match VarMap.find_opt a var_map.vars with
      | None -> raise (CodeGenError ("Variable " ^ a ^ " is undefined."))
      | Some (offset, t) ->
          ( match t with
          | IntType ->
              output ("movl  " ^ string_of_int offset ^ "(%ebp),  %eax\n")
          | ArrayType (_, _) ->
              output "movl    %ebp, %eax\n";
              output ("addl  $" ^ string_of_int offset ^ ", %eax\n")
          | PointerType _ ->
              output ("movl  " ^ string_of_int offset ^ "(%ebp),  %eax\n")
          | x ->
              CodeGenError ("Unsupported type in VarExp." ^ print_data_type x)
              |> raise );
          t )
  | ArrayIndexExp (exp1, exp2) -> (
      (* exp1 must be of type Array, in the current implementation types are evaluated
       * during code generation, the evaluation result and step size also depends on
       * the type of exp1, so we need to evaluate exp1 first.*)
      let t = generate_expression ctx var_map exp1 in
      match t with
      | ArrayType (child_type, size) ->
          let _, temp_loc, var_map = allocate_stack ctx var_map 4 in
          output ("movl    %eax, " ^ temp_loc ^ "\n");
          let t2 = generate_expression ctx var_map exp2 in
          ignore t2;
          output
            ( "imul    $"
            ^ string_of_int (get_data_size child_type)
            ^ ", %eax # nidex array index\n" );
          output ("movl    " ^ temp_loc ^ ", %ecx\n");
          output "addl    %ecx, %eax\n";
          if not (is_type_array child_type) then
            output "movl    (%eax), %eax# index array end (value)\n"
          else output " # index array end (addr, already in eax)\n";
          child_type
      | _ ->
          raise
            (CodeGenError
               ( "Target is not array type: " ^ print_expression 0 exp1
               ^ ", actual type: " ^ print_data_type t )) )
  | DereferenceExp exp -> (
      let t = generate_expression ctx var_map exp in
      if not (is_type_pointer t) then
        raise (CodeGenError "Only pointer type can be dereferenced.")
      else output "movl    (%eax), %eax\n";
      match t with
      | PointerType inner -> inner
      | _ -> raise (CodeGenError "Expecting a pointer type in DereferenceExp.")
      )
  | AddressOfExp (VarExp a) -> (
      match VarMap.find_opt a var_map.vars with
      | None -> raise (CodeGenError ("Variable " ^ a ^ " is undefined."))
      | Some (offset, t) -> (
          match t with
          | IntType | ArrayType (_, _) | PointerType _ ->
              output
                ("movl  %ebp, %eax\naddl $" ^ string_of_int offset ^ ",  %eax\n");
              PointerType t
          | _ -> raise (CodeGenError "Illegal type in AddressOfExp.") ) )
  | AddressOfExp (ArrayIndexExp (exp1, exp2)) -> (
      (* TODO remove this duplicate code with the ArrayIndexExp code above. *)
      let t = generate_expression ctx var_map exp1 in
      match t with
      | ArrayType (child_type, size) ->
          output "pushl    %eax # index array addr\n";
          let t2 = generate_expression ctx var_map exp2 in
          ignore t2;
          output
            ( "imul    $"
            ^ string_of_int (get_data_size child_type)
            ^ ", %eax # index array index\n" );
          output "popl    %ecx\n";
          output "addl    %eax, %ecx\n";
          output "movl    %ecx, %eax\n";
          PointerType child_type
      | _ -> raise (CodeGenError "Expecting an array type in ArrayIndexExp.") )
  | AddressOfExp _ ->
      raise (CodeGenError "Can only take address of array element or varaible.")
  | ConstantIntExp n ->
      output ("movl    $" ^ string_of_int n ^ ", %eax\n");
      IntType
  | FunctionCallExp (fname, exps) ->
      generate_f_call ctx var_map fname exps;
      (* TODO assume function always return int. *)
      IntType
  | NegateOp exp ->
      generate_expression ctx var_map exp |> ignore;
      output "neg    %eax\n";
      IntType
  | LogicalNegateOp exp ->
      generate_expression ctx var_map exp |> ignore;
      output "cmpl    $0, %eax\nmovl     $0, %eax\nsete    %al\n";
      IntType
  | ComplementOp exp ->
      generate_expression ctx var_map exp |> ignore;
      output "not    %eax\n";
      IntType
  | GroupedExpression exp -> generate_expression ctx var_map exp
  | ConstantCharExp a -> raise (CodeGenError "Char nimplemented")
  | ConstantStringExp a -> raise (CodeGenError "String unimplemented")
  | ConstantFloatExp a -> raise (CodeGenError "Float unimplemented")
  | AdditionExp (exp1, exp2) ->
      generate_expression ctx var_map exp1 |> ignore;
      output "push    %eax\n";
      let t = generate_expression ctx var_map exp2 in
      output "pop    %ecx\n";
      output "addl    %ecx, %eax\n";
      t
  | MinusExp (exp1, exp2) ->
      generate_expression ctx var_map exp2 |> ignore;
      output "push    %eax\n";
      let t = generate_expression ctx var_map exp1 in
      output "pop    %ecx\n";
      output "subl    %ecx, %eax\n";

      t
  | MultiExp (exp1, exp2) ->
      generate_expression ctx var_map exp1 |> ignore;
      output "push    %eax\n";
      let t = generate_expression ctx var_map exp2 in
      output "pop    %ecx\n";
      output "imul    %ecx, %eax\n";
      t
  | DivideExp (exp1, exp2) ->
      generate_expression ctx var_map exp2 |> ignore;
      output "push    %eax\n";
      let t = generate_expression ctx var_map exp1 in
      output "cdq\n";
      output "pop    %ecx\n";
      output "idvl    %ecx\n";
      t
  | EqualExp (exp1, exp2) ->
      generate_relational_expression output "sete" exp1 exp2
  | NotEqualExp (exp1, exp2) ->
      generate_relational_expression output "setne" exp1 exp2
  | GreaterOrEqualExp (exp1, exp2) ->
      generate_relational_expression output "setge" exp1 exp2
  | GreaterExp (exp1, exp2) ->
      generate_relational_expression output "setg" exp1 exp2
  | LessOrEqualExp (exp1, exp2) ->
      generate_relational_expression output "setle" exp1 exp2
  | LessExp (exp1, exp2) ->
      generate_relational_expression output "setl" exp1 exp2
  | OrExp (exp1, exp2) ->
      generate_expression ctx var_map exp1 |> ignore;
      let clause_label = get_unique_label "_clause2" in
      let end_label = get_unique_label "_end" in
      output ("cmpl    $0, %eax\nje " ^ clause_label ^ "\n");
      output ("movl    $1, %eax\njmp " ^ end_label ^ "\n");
      output (clause_label ^ ":\n");
      generate_expression ctx var_map exp2 |> ignore;
      output "cmpl    $0, %eax\nmovl    $0, %eax\nsetne    %al\n";
      output (end_label ^ ":\n");
      IntType
  | AndExp (exp1, exp2) ->
      generate_expression ctx var_map exp1 |> ignore;
      let clause_label = get_unique_label "_clause2" in
      let end_label = get_unique_label "_end" in
      output ("cmpl    $0, %eax\njne " ^ clause_label ^ "\n");
      output ("movl    $0, %eax\njmp " ^ end_label ^ "\n");
      output (clause_label ^ ":\n");
      generate_expression ctx var_map exp2 |> ignore;
      output "cmpl    $0, %eax\nmovl    $0, %eax\nsetne    %al\n";
      output (end_label ^ ":\n");
      IntType
  | ConditionExp (exp1, exp2, exp3) ->
      generate_expression ctx var_map exp1 |> ignore;
      let cond_label = get_unique_label "_cond" in
      let cond_end_label = get_unique_label "_condend" in
      output ("cmpl    $0, %eax\nje    " ^ cond_label ^ "\n");
      let t = generate_expression ctx var_map exp2 in
      output ("jmp    " ^ cond_end_label ^ "\n");
      output (cond_label ^ ":\n");
      generate_expression ctx var_map exp3 |> ignore;
      output (cond_end_label ^ ":\n");
      t
  | AssignExp (VarExp a, exp) -> (
      match VarMap.find_opt a var_map.vars with
      | None -> raise (CodeGenError ("Variable " ^ a ^ " is undefined."))
      | Some (offset, t) ->
          let t = generate_expression ctx var_map exp in
          output ("movl  %eax, " ^ string_of_int offset ^ "(%ebp)\n");
          t )
  | AssignExp (ArrayIndexExp (exp1, exp2), exp_r) -> (
      (* TODO Remove ruplicate code with the other ArrayIndexExp code. *)
      let t = generate_expression ctx var_map exp1 in
      output "pushl    %eax # array assign addr\n";
      match t with
      | ArrayType (element_type, size) ->
          if is_type_array element_type then
            raise (CodeGenError "Only 1-D array is assignable.")
          else generate_expression ctx var_map exp2 |> ignore;
          output
            ( "imul    $"
            ^ string_of_int (get_data_size element_type)
            ^ ", %eax  # array assign index\n" );
          output "popl    %ecx\n";
          output "addl    %eax, %ecx\n";
          output "pushl    %ecx\n";
          generate_expression ctx var_map exp_r |> ignore;
          output "popl    %ecx\n";
          output "movl    %eax, (%ecx) # array assign end\n";
          element_type
      | a ->
          CodeGenError ("Type is not assignable: " ^ print_data_type a) |> raise
      )
  | AssignExp (_, _) ->
      CodeGenError "Left hand side is not assignable!" |> raise

(* Updates the continue_label and break_label fields in the given var_map. *)
let update_break_continue_label (var_map : var_map_t) (break : string)
    (continue : string) : var_map_t =
  { var_map with continue_label = continue; break_label = break }

(* Generates assmebly code for a block item. *)
let rec generate_block_item (ctx : context_t) (var_map : var_map_t)
    (st : block_item_t) : var_map_t =
  let output = ctx.output in
  let get_unique_label = ctx.get_unique_label in
  match st with
  | StatementItem (ReturnStatement exp) ->
      generate_expression ctx var_map exp |> ignore;
      output ("jmp  " ^ var_map.function_return_label ^ "    # return \n");
      var_map
  | StatementItem (ExpressionStatement (Some exp)) ->
      generate_expression ctx var_map exp |> ignore;
      var_map
  | StatementItem (ExpressionStatement None) -> var_map
  | StatementItem (ConditionalStatement (exp, st1, Some st2)) ->
      let cond_label = get_unique_label "_cond" in
      let cond_end_label = get_unique_label "_condend" in
      generate_expression ctx var_map exp |> ignore;
      output ("cmpl    $0, %eax\nje    " ^ cond_label ^ "\n");
      let var_map = generate_block_item ctx var_map (StatementItem st1) in
      output ("jmp    " ^ cond_end_label ^ "\n");
      output (cond_label ^ ":\n");
      generate_block_item ctx var_map (StatementItem st2) |> ignore;
      output (cond_end_label ^ ":\n");
      var_map
  | StatementItem (ConditionalStatement (exp, st1, None)) ->
      let cond_end_label = get_unique_label "_condend" in
      generate_expression ctx var_map exp |> ignore;
      output ("cmpl    $0, %eax\nje    " ^ cond_end_label ^ "\n");
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
      generate_expression ctx var_map exp2 |> ignore;
      output ("cmpl    $0, %eax\nje    " ^ end_label ^ "\n");
      generate_block_item ctx
        (update_break_continue_label var_map break_label continue_label)
        (StatementItem st)
      |> ignore;
      output (continue_label ^ ":\n");
      generate_update_esp ctx var_map;
      ( match exp3_opt with
      | None -> ()
      | Some exp ->
          generate_expression ctx var_map exp |> ignore;
          () );
      output ("jmp    " ^ cond_label ^ "\n");
      output (end_label ^ ":\n");
      generate_update_esp ctx var_map;
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
      generate_expression ctx condition_var_map exp2 |> ignore;
      output ("cmpl    $0, %eax\nje    " ^ end_label ^ "\n");
      generate_block_item ctx
        (update_break_continue_label condition_var_map break_label
           continue_label)
        (StatementItem st)
      |> ignore;
      output (continue_label ^ ":\n");
      generate_update_esp ctx condition_var_map;
      ( match exp3_opt with
      | None -> ()
      | Some exp -> generate_expression ctx condition_var_map exp |> ignore );
      output ("jmp    " ^ cond_label ^ "\n");
      output (end_label ^ ":\n");
      generate_update_esp ctx condition_var_map;
      var_map
  | StatementItem BreakStatement ->
      if var_map.break_label = "" then
        raise (CodeGenError "Illegal break, no context.");
      output ("jmp    " ^ var_map.break_label ^ "\n");
      var_map
  | StatementItem ContinueStatement ->
      if var_map.continue_label = "" then
        raise (CodeGenError "Illegal jump, no context.");
      output ("jmp    " ^ var_map.continue_label ^ "\n");
      var_map
  | StatementItem (WhileStatement (exp, st)) ->
      let cond_label = get_unique_label "_whilecond" in
      let end_label = get_unique_label "_whileend" in
      let break_label = end_label in
      let continue_label = cond_label in
      output (cond_label ^ ":\n");
      generate_update_esp ctx var_map;
      generate_expression ctx var_map exp |> ignore;
      output ("cmpl    $0, %eax\nje    " ^ end_label ^ "\n");
      generate_block_item ctx
        (update_break_continue_label var_map break_label continue_label)
        (StatementItem st)
      |> ignore;
      output ("jmp    " ^ cond_label ^ "\n");
      output (end_label ^ ":\n");
      generate_update_esp ctx var_map;
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
      generate_update_esp ctx var_map;
      generate_expression ctx var_map exp |> ignore;
      output ("cmpl    $0, %eax\njne    " ^ begin_label ^ "\n");
      output (end_label ^ ":\n");
      generate_update_esp ctx var_map;
      var_map
  | StatementItem (CompoundStatement items) ->
      (* Entering a new scope, so clear the cur_scope_vars, but we ignore the inner var_map returned. *)
      generate_block_statements ctx
          { var_map with cur_scope_vars = VarMap.empty }
          items |> ignore;
      var_map
  | DeclareItem (DeclareStatement (data_type, a, exp_opt)) -> (
      ( match exp_opt with
      | None -> output "movl    $0, %eax\n"
      | Some exp -> generate_expression ctx var_map exp |> ignore );
      (* allocate the data block on stack. *)
      let start_offset, _, var_map = allocate_stack ctx var_map (get_data_size data_type) in
      (* output ("subl   $" ^ string_of_int (get_data_size data_type) ^ ", %esp\n");
      (* Start from the lower address. *)
      let start_offset = var_map.index - get_data_size data_type in *)
      output ("movl    %eax, " ^ string_of_int start_offset ^ "(%ebp)\n");
      match VarMap.find_opt a var_map.cur_scope_vars with
      | Some (_, _) ->
          raise
            (CodeGenError ("Var " ^ a ^ " is already defined in current scope!"))
      | None ->
          {
            var_map with
            vars = VarMap.add a (start_offset, data_type) var_map.vars;
            cur_scope_vars =
              VarMap.add a (start_offset, data_type) var_map.cur_scope_vars;
            index = start_offset;
          } )

(* Generates the assembly code for a list of block items. *)
and generate_block_statements (ctx : context_t) (var_map : var_map_t)
    (sts : block_item_t list) : var_map_t =
  List.fold_left (generate_block_item ctx) var_map sts

let generate_function (ctx : context_t) (f : function_t) : var_map_t =
  (* Generates the var_map with the references to the function arguments. *)
  let rec generate_f_var_map var_map fname params index =
    match params with
    | [] -> var_map
    | a :: r ->
        generate_f_var_map
          { var_map with
            (* TODO We assumed args are always int. *)
            vars = VarMap.add a (index, IntType) var_map.vars;
            break_label = "";
            continue_label = "";
          }
          fname r (index + 4)
  in

  let var_map =
    {
      vars = VarMap.empty;
      cur_scope_vars = VarMap.empty;
      (* Start the offset as 0, since %ebp = %esp. *)
      index = 0;
      break_label = "";
      continue_label = "";
      function_return_label = "";
    }
  in
  match f with
  | IntFunction (fname, params, items_opt) -> (
      match items_opt with
      | None -> var_map
      | Some items ->
          let output = ctx.output in
          output ("_" ^ fname ^ ":\n");
          (* Saving the previous stack start point and use esp as the new stack start. *)
          output "push    %ebp\nmovl    %esp, %ebp\n";
          (* Reset the index at the function beginning. *)
          ctx.set_min_index 0;
          (* This is a little hacky, we need to generate the output then get the stack size,
           * but we need to output stack allocation command first, so we use a temporary buffer
           * for each function. *)
          let fun_buf = Buffer.create 32 in
          let return_label = ctx.get_unique_label "_fun_return" in
          let ctx =
            { ctx with output = (fun a -> Buffer.add_string fun_buf a) }
          in
          let var_map = generate_f_var_map var_map fname params 8 in
          (* The block statements will be in the temporary buffer fun_buf. *)
          let var_map = {var_map with function_return_label = return_label } in
          let res = generate_block_statements ctx var_map items in
          output ("addl    $" ^ string_of_int (ctx.get_min_index ()) ^ ", %esp\n");
          output (Buffer.contents fun_buf);
          (* Return logic *)
          output (return_label ^ ":\n");
          output ("addl    $" ^ string_of_int (-(ctx.get_min_index ())) ^ ", %esp\n");
          output "movl    %ebp, %esp\npop    %ebp\n";
          output "ret\n";
          res )

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
      is_64_bit = false;
      set_min_index = (fun a -> min_index := a);
      get_min_index = (fun () -> !min_index);
    }
  in
  ctx.output ".globl _main\n";
  match ast with
  | Program fns ->
      List.iter (fun f -> generate_function ctx f |> ignore) fns;
      Buffer.contents buf
