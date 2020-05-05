open Lexer
open Parser
open Util
module VarMap = Map.Make (String)

exception CodeGenError of string

type var_map_t = {
  vars : int VarMap.t;
  cur_scope_vars : int VarMap.t;
  index : int;
  break_label : string;
  continue_label : string;
}

type context_t = {
  output : string -> unit;
  get_unique_label : string -> string;
}

(* Hacky solution per Nora's article to add padding so that function stack is 16
 * byte aligned. This is for MacOs only. *)
let add_function_call_padding (output : string -> unit) (num_args : int) : unit
    =
  output "movl    %esp, %eax\n";
  output ("subl $" ^ string_of_int (4 * (num_args + 1)) ^ ", %eax\n");
  output "xorl %edx, %edx\nmovl $0x20, %ecx\nidivl %ecx\n";
  output "subl %edx, %esp\npushl %edx\n"

let remove_function_call_padding (ctx : context_t) : unit =
  ctx.output "popl %edx\naddl %edx, %esp\n"

let generate_update_esp (ctx : context_t) (var_map : var_map_t) : unit =
  ctx.output "movl    %ebp, %eax\n";
  ctx.output ("subl    $" ^ string_of_int (-var_map.index - 4) ^ ",  %eax\n");
  ctx.output "movl    %eax, %esp\n"

(* Generates the assembly code for a specific expression. *)
let rec generate_expression (ctx : context_t) (var_map : var_map_t)
    (exp : expression_t) : unit =
  let output = ctx.output in
  let get_unique_label = ctx.get_unique_label in
  let rec generate_f_call var_map fname exps num_args =
    match exps with
    | [] ->
        ctx.output ("call    _" ^ fname ^ "\n");
        (* Function returned, remove arguments from stack. *)
        ctx.output ("addl    $" ^ string_of_int (4 * num_args) ^ ", %esp\n");
        (* Remove the padding *)
        remove_function_call_padding ctx
    | a :: r ->
        generate_expression ctx var_map a;
        ctx.output "push   %eax\n";
        generate_f_call var_map fname r num_args
  in

  let generate_relational_expression output command exp1 exp2 =
    generate_expression ctx var_map exp1;
    output "push    %eax\n";
    generate_expression ctx var_map exp2;
    output "pop    %ecx\n";
    output "cmpl   %eax, %ecx\n";
    output "movl   $0, %eax\n";
    output (command ^ "   %al\n")
  in
  match exp with
  | VarExp a -> (
      match VarMap.find_opt a var_map.vars with
      | None -> raise (CodeGenError ("Variable " ^ a ^ " is undefined."))
      | Some offset ->
          output ("movl  " ^ string_of_int offset ^ "(%ebp),  %eax\n") )
  | ArrayIndexExp(_,_) -> raise (CodeGenError "Array index unimplemented")
  | ConstantIntExp n -> output ("movl    $" ^ string_of_int n ^ ", %eax\n")
  | FunctionCallExp (fname, exps) ->
      add_function_call_padding output (List.length exps);
      generate_f_call var_map fname exps (List.length exps);
      ()
  | NegateOp exp ->
      generate_expression ctx var_map exp;
      output "neg    %eax\n"
  | LogicalNegateOp exp ->
      generate_expression ctx var_map exp;
      output "cmpl    $0, %eax\nmovl     $0, %eax\nsete    %al\n"
  | ComplementOp exp ->
      generate_expression ctx var_map exp;
      output "not    %eax\n"
  | GroupedExpression exp -> generate_expression ctx var_map exp
  | ConstantCharExp(a)  -> raise (CodeGenError "Char nimplemented")
  | ConstantStringExp(a) -> raise (CodeGenError "String unimplemented")
  | ConstantFloatExp(a) -> raise (CodeGenError "Float unimplemented")
  | AdditionExp (exp1, exp2) ->
      generate_expression ctx var_map exp1;
      output "push    %eax\n";
      generate_expression ctx var_map exp2;
      output "pop    %ecx\n";
      output "addl    %ecx, %eax\n"
  | MinusExp (exp1, exp2) ->
      generate_expression ctx var_map exp2;
      output "push    %eax\n";
      generate_expression ctx var_map exp1;
      output "pop    %ecx\n";
      output "subl    %ecx, %eax\n"
  | MultiExp (exp1, exp2) ->
      generate_expression ctx var_map exp1;
      output "push    %eax\n";
      generate_expression ctx var_map exp2;
      output "pop    %ecx\n";
      output "imul    %ecx, %eax\n"
  | DivideExp (exp1, exp2) ->
      generate_expression ctx var_map exp2;
      output "push    %eax\n";
      generate_expression ctx var_map exp1;
      output "cdq\n";
      output "pop    %ecx\n";
      output "idvl    %ecx\n"
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
      generate_expression ctx var_map exp1;
      let clause_label = get_unique_label "_clause2" in
      let end_label = get_unique_label "_end" in
      output ("cmpl    $0, %eax\nje " ^ clause_label ^ "\n");
      output ("movl    $1, %eax\njmp " ^ end_label ^ "\n");
      output (clause_label ^ ":\n");
      generate_expression ctx var_map exp2;
      output "cmpl    $0, %eax\nmovl    $0, %eax\nsetne    %al\n";
      output (end_label ^ ":\n")
  | AndExp (exp1, exp2) ->
      generate_expression ctx var_map exp1;
      let clause_label = get_unique_label "_clause2" in
      let end_label = get_unique_label "_end" in
      output ("cmpl    $0, %eax\njne " ^ clause_label ^ "\n");
      output ("movl    $0, %eax\njmp " ^ end_label ^ "\n");
      output (clause_label ^ ":\n");
      generate_expression ctx var_map exp2;
      output "cmpl    $0, %eax\nmovl    $0, %eax\nsetne    %al\n";
      output (end_label ^ ":\n")
  | ConditionExp (exp1, exp2, exp3) ->
      generate_expression ctx var_map exp1;
      let cond_label = get_unique_label "_cond" in
      let cond_end_label = get_unique_label "_condend" in
      output ("cmpl    $0, %eax\nje    " ^ cond_label ^ "\n");
      generate_expression ctx var_map exp2;
      output ("jmp    " ^ cond_end_label ^ "\n");
      output (cond_label ^ ":\n");
      generate_expression ctx var_map exp3;
      output (cond_end_label ^ ":\n")
  | AssignExp (VarExp a, exp) -> (
      match VarMap.find_opt a var_map.vars with
      | None -> raise (CodeGenError ("Variable " ^ a ^ " is undefined."))
      | Some offset ->
          generate_expression ctx var_map exp;
          output ("movl  %eax, " ^ string_of_int offset ^ "(%ebp)\n") )
  | AssignExp (ArrayIndexExp (exp1, exp2), exp_r) ->
      CodeGenError "Array assignment not supported yet!." |> raise
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
      generate_expression ctx var_map exp;
      output "movl    %ebp, %esp\npop    %ebp\n";
      output "ret\n";
      var_map
  | StatementItem (ExpressionStatement (Some exp)) ->
      generate_expression ctx var_map exp;
      var_map
  | StatementItem (ExpressionStatement None) -> var_map
  | StatementItem (ConditionalStatement (exp, st1, Some st2)) ->
      let cond_label = get_unique_label "_cond" in
      let cond_end_label = get_unique_label "_condend" in
      generate_expression ctx var_map exp;
      output ("cmpl    $0, %eax\nje    " ^ cond_label ^ "\n");
      let var_map = generate_block_item ctx var_map (StatementItem st1) in
      output ("jmp    " ^ cond_end_label ^ "\n");
      output (cond_label ^ ":\n");
      generate_block_item ctx var_map (StatementItem st2) |> ignore;
      output (cond_end_label ^ ":\n");
      var_map
  | StatementItem (ConditionalStatement (exp, st1, None)) ->
      let cond_end_label = get_unique_label "_condend" in
      generate_expression ctx var_map exp;
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
          generate_expression ctx var_map exp;
          () );
      output (cond_label ^ ":\n");
      generate_expression ctx var_map exp2;
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
          generate_expression ctx var_map exp;
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
      generate_expression ctx condition_var_map exp2;
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
      | Some exp -> generate_expression ctx condition_var_map exp );
      output ("jmp    " ^ cond_label ^ "\n");
      output (end_label ^ ":\n");
      generate_update_esp ctx condition_var_map;
      (* The variables declared in the block will be deacllocated automatically inside generate_block_item. *)
      (* The condition expressions has its own scope, needs to be deallocated here. *)
      output
        ( "addl $"
        ^ string_of_int (4 * VarMap.cardinal condition_var_map.cur_scope_vars)
        ^ ", %esp\n" );
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
      generate_expression ctx var_map exp;
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
      generate_expression ctx var_map exp;
      output ("cmpl    $0, %eax\njne    " ^ begin_label ^ "\n");
      output (end_label ^ ":\n");
      generate_update_esp ctx var_map;
      var_map
  | StatementItem (CompoundStatement items) ->
      (* Entering a new scope, so clear the cur_scope_vars, but we ignore the inner var_map returned. *)
      let inner_var_map =
        generate_block_statements ctx
          { var_map with cur_scope_vars = VarMap.empty }
          items
      in
      (* Move stack pointer %esp back by number of allocations in the inner scope. This is like deallocating inner variables.*)
      output
        ( "addl $"
        ^ string_of_int (4 * VarMap.cardinal inner_var_map.cur_scope_vars)
        ^ ", %esp\n" );
      var_map
  | DeclareItem (DeclareStatement (data_type, a, exp_opt)) -> (
      let rec get_data_size (t : data_type_t) : int =
        match t with
        | IntType -> 4
        | ArrayType (t2, sizes) ->
            List.fold_left (fun acc a -> acc * a) 1 sizes * get_data_size t2
        | _ -> raise (CodeGenError "Unsupported data type.")
      in
      ( match exp_opt with
      | None -> output "movl    $0, %eax\n"
      | Some exp -> generate_expression ctx var_map exp );
      output "push    %eax\n";
      match VarMap.find_opt a var_map.cur_scope_vars with
      | Some x ->
          raise
            (CodeGenError ("Var " ^ a ^ " is already defined in current scope!"))
      | None ->
          {
            var_map with
            vars = VarMap.add a var_map.index var_map.vars;
            cur_scope_vars = VarMap.add a var_map.index var_map.cur_scope_vars;
            index = var_map.index - get_data_size data_type;
          } )

(* Generates the assembly code for a list of block items. *)
and generate_block_statements (ctx : context_t) (var_map : var_map_t)
    (sts : block_item_t list) : var_map_t =
  List.fold_left (generate_block_item ctx) var_map sts

let generate_function (ctx : context_t) (f : function_t) : var_map_t =
  let output = ctx.output in

  (* index is the next available offset to esp to save new local variables, at the
   * beginning of a function, the index is one word (4 bytes) after the esp register. *)

  (* Generates the var_map with the references to the function arguments. *)
  let rec generate_f_var_map var_map fname params index =
    match params with
    | [] -> var_map
    | a :: r ->
        generate_f_var_map
          {
            vars = VarMap.add a index var_map.vars;
            cur_scope_vars = var_map.cur_scope_vars;
            index = var_map.index;
            break_label = "";
            continue_label = "";
          }
          fname r (index + 4)
  in

  let var_map =
    {
      vars = VarMap.empty;
      cur_scope_vars = VarMap.empty;
      index = -4;
      break_label = "";
      continue_label = "";
    }
  in
  match f with
  | IntFunction (fname, params, items_opt) -> (
      match items_opt with
      | None -> var_map
      | Some items ->
          output ("_" ^ fname ^ ":\n");
          (* Saving the previous stack start point and use esp as the new stack start. *)
          output "push    %ebp\nmovl    %esp, %ebp\n";
          let var_map = generate_f_var_map var_map fname params 8 in
          generate_block_statements ctx var_map items )

(* Generates the assembly code as a string given the ast in Parser.program_t type. *)
let generate_assembly (ast : program_t) : string =
  let buf = Buffer.create 32 in
  let count = ref 0 in
  let ctx =
    {
      output = Buffer.add_string buf;
      get_unique_label = get_unique_label count;
    }
  in
  ctx.output ".globl _main\n";
  match ast with
  | Program fns ->
      List.iter (fun f -> generate_function ctx f |> ignore) fns;
      Buffer.contents buf
