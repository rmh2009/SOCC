open Lexer
open Parser
open Util

module VarMap = Map.Make(String)

exception CodeGenError of string

type var_map_t = {
  vars: int VarMap.t ;
  cur_scope_vars:
    int VarMap.t;
    index : int;
    break_label: string;
    continue_label: string;
}

(* Generates the assembly code as a string given the ast in Parser.program_t type. *)
let generate_assembly ast =
  let buf = Buffer.create 32 in
  let count = ref 0 in
  let rec generate_expression var_map exp =
    let generate_relational_expression buf command exp1 exp2=
      generate_expression var_map exp1;
      Buffer.add_string buf "push    %eax\n";
      generate_expression var_map exp2;
      Buffer.add_string buf "pop    %ecx\n";
      Buffer.add_string buf "cmpl   %eax, %ecx\n";
      Buffer.add_string buf "movl   $0, %eax\n";
      Buffer.add_string buf (command ^ "   %al\n")
    in
    match exp with
        | VarExp a ->
            (match VarMap.find_opt a var_map.vars with
            | None -> raise (CodeGenError ("Variable " ^ a ^ " is undefined."))
            | Some offset ->
                Buffer.add_string buf ("movl  " ^ (string_of_int offset) ^ "(%ebp),  %eax\n"))
        | ConstantIntExp n -> Buffer.add_string buf ("movl    $" ^ (string_of_int n) ^ ", %eax\n" )
        | NegateOp exp ->
            generate_expression var_map exp;
            Buffer.add_string buf "neg    %eax\n"
        | LogicalNegateOp exp ->
            generate_expression var_map exp;
            Buffer.add_string buf "cmpl    $0, %eax\nmovl     $0, %eax\nsete    %al\n"
        | ComplementOp exp ->
            generate_expression var_map exp;
            Buffer.add_string buf "not    %eax\n"
        | GroupedExpression exp ->
            generate_expression var_map exp
        | AdditionExp (exp1, exp2) ->
            generate_expression var_map exp1;
            Buffer.add_string buf "push    %eax\n";
            generate_expression var_map exp2;
            Buffer.add_string buf "pop    %ecx\n";
            Buffer.add_string buf "addl    %ecx, %eax\n"
        | MinusExp (exp1, exp2) ->
            generate_expression var_map exp2;
            Buffer.add_string buf "push    %eax\n";
            generate_expression var_map exp1;
            Buffer.add_string buf "pop    %ecx\n";
            Buffer.add_string buf "subl    %ecx, %eax\n"
        | MultiExp (exp1, exp2) ->
            generate_expression var_map exp1;
            Buffer.add_string buf "push    %eax\n";
            generate_expression var_map exp2;
            Buffer.add_string buf "pop    %ecx\n";
            Buffer.add_string buf "imul    %ecx, %eax\n"
        | DivideExp (exp1, exp2) ->
            generate_expression var_map exp2;
            Buffer.add_string buf "push    %eax\n";
            generate_expression var_map exp1;
            Buffer.add_string buf "cdq\n";
            Buffer.add_string buf "pop    %ecx\n";
            Buffer.add_string buf "idvl    %ecx\n"
        | EqualExp (exp1, exp2) ->
            generate_relational_expression buf "sete" exp1 exp2
        | NotEqualExp (exp1, exp2) ->
            generate_relational_expression buf "setne" exp1 exp2
        | GreaterOrEqualExp (exp1, exp2) ->
            generate_relational_expression buf "setge" exp1 exp2
        | GreaterExp (exp1, exp2) ->
            generate_relational_expression buf "setg" exp1 exp2
        | LessOrEqualExp (exp1, exp2) ->
            generate_relational_expression buf "setle" exp1 exp2
        | LessExp (exp1, exp2) ->
            generate_relational_expression buf "setl" exp1 exp2
        | OrExp (exp1, exp2) ->
            generate_expression var_map exp1;
            let clause_label = get_unique_label "_clause2" count in
            let end_label = get_unique_label "_end" count in
            Buffer.add_string buf ("cmpl    $0, %eax\nje " ^ clause_label ^ "\n");
            Buffer.add_string buf ("movl    $1, %eax\njmp " ^ end_label ^ "\n");
            Buffer.add_string buf (clause_label ^ ":\n");
            generate_expression var_map exp2;
            Buffer.add_string buf ("cmpl    $0, %eax\nmovl    $0, %eax\nsetne    %al\n");
            Buffer.add_string buf (end_label ^ ":\n")
        | AndExp (exp1, exp2) ->
            generate_expression var_map exp1;
            let clause_label = get_unique_label "_clause2" count in
            let end_label = get_unique_label "_end" count in
            Buffer.add_string buf ("cmpl    $0, %eax\njne " ^ clause_label ^ "\n");
            Buffer.add_string buf ("movl    $0, %eax\njmp " ^ end_label ^ "\n");
            Buffer.add_string buf (clause_label ^ ":\n");
            generate_expression var_map exp2;
            Buffer.add_string buf ("cmpl    $0, %eax\nmovl    $0, %eax\nsetne    %al\n");
            Buffer.add_string buf (end_label ^ ":\n")
        | ConditionExp (exp1, exp2, exp3) ->
            generate_expression var_map exp1;
            let cond_label = get_unique_label "_cond" count in
            let cond_end_label = get_unique_label "_condend" count in
            Buffer.add_string buf ("cmpl    $0, %eax\nje    " ^ cond_label ^ "\n");
            generate_expression var_map exp2;
            Buffer.add_string buf ("jmp    " ^ cond_end_label ^ "\n");
            Buffer.add_string buf (cond_label ^ ":\n");
            generate_expression var_map exp3;
            Buffer.add_string buf (cond_end_label ^ ":\n");
        | AssignExp (a, exp) ->
            (match VarMap.find_opt a var_map.vars with
            | None -> raise (CodeGenError ("Variable " ^ a ^ " is undefined."))
            | Some offset -> 
                generate_expression var_map exp;
                Buffer.add_string buf ("movl  %eax, " ^ (string_of_int offset) ^ "(%ebp)\n"))
  in

  let update_break_continue_label var_map break continue =
    { vars = var_map.vars;
      cur_scope_vars = var_map.cur_scope_vars;
      index = var_map.index;
      continue_label = continue;
      break_label = break}
  in
  
  (* This is used to update the stack top pointer %esp after break/continue. This will always
   * be on top of the current var_map.index.*)
  let generate_update_esp buf var_map =
    Buffer.add_string buf "movl    %ebp, %eax\n";
    Buffer.add_string buf ("subl    $" ^ (string_of_int (-var_map.index - 4)) ^ ",  %eax\n");
    Buffer.add_string buf "movl    %eax, %esp\n"
  in

  let rec generate_block_item var_map st =
    match st with
    | StatementItem (ReturnStatement exp) ->
        generate_expression var_map exp;
        Buffer.add_string buf "movl    %ebp, %esp\npop    %ebp\n";
        Buffer.add_string buf "ret\n";
        var_map
    | StatementItem (ExpressionStatement (Some exp)) ->
        generate_expression var_map exp;
        var_map
    | StatementItem (ExpressionStatement None) ->
        var_map
    | StatementItem (ConditionalStatement (exp, st1, Some st2)) ->
        let cond_label = get_unique_label "_cond" count in
        let cond_end_label = get_unique_label "_condend" count in
        generate_expression var_map exp;
        Buffer.add_string buf ("cmpl    $0, %eax\nje    " ^ cond_label ^ "\n");
        let var_map = generate_block_item var_map (StatementItem(st1)) in
        Buffer.add_string buf ("jmp    " ^ cond_end_label ^ "\n");
        Buffer.add_string buf (cond_label ^ ":\n");
        generate_block_item var_map (StatementItem(st2));
        Buffer.add_string buf (cond_end_label ^ ":\n");
        var_map
    | StatementItem (ConditionalStatement (exp, st1, None)) ->
        let cond_end_label = get_unique_label "_condend" count in
        generate_expression var_map exp;
        Buffer.add_string buf ("cmpl    $0, %eax\nje    " ^ cond_end_label ^ "\n");
        let var_map = generate_block_item var_map (StatementItem(st1)) in
        Buffer.add_string buf (cond_end_label ^ ":\n");
        var_map
    | StatementItem (ForStatement(exp1_opt, exp2, exp3_opt, st)) ->
        let cond_label = get_unique_label "_forcond" count in
        let end_label = get_unique_label "_forend" count in
        let break_label = end_label in
        let continue_label = get_unique_label "_forcontinue" count in
        (match exp1_opt with
        | None -> ()
        | Some exp -> generate_expression var_map exp; ());
        Buffer.add_string buf (cond_label ^ ":\n");
        generate_expression var_map exp2;
        Buffer.add_string buf ("cmpl    $0, %eax\nje    " ^ end_label ^ "\n");
        let inner_var_map = generate_block_item (update_break_continue_label var_map break_label continue_label) 
        (StatementItem(st)) in
        Buffer.add_string buf (continue_label ^ ":\n");
        generate_update_esp buf var_map;
        (match exp3_opt with
        | None -> ()
        | Some exp -> generate_expression var_map exp; ());
        Buffer.add_string buf ("jmp    " ^ cond_label ^ "\n");
        Buffer.add_string buf (end_label ^ ":\n");
        generate_update_esp buf var_map;
        var_map

    | StatementItem (ForDeclStatement(declare, exp2, exp3_opt, st)) ->
        let cond_label = get_unique_label "_forcond" count in
        let end_label = get_unique_label "_forend" count in
        let break_label = end_label in
        let continue_label = get_unique_label "_forcontinue" count in
        let condition_var_map = generate_block_item
        {vars = var_map.vars;
        cur_scope_vars = VarMap.empty;
        index = var_map.index;
        break_label = "";
        continue_label = ""} (DeclareItem(declare)) in
        Buffer.add_string buf (cond_label ^ ":\n");
        generate_expression condition_var_map exp2;
        Buffer.add_string buf ("cmpl    $0, %eax\nje    " ^ end_label ^ "\n");
        let inner_var_map = generate_block_item (update_break_continue_label condition_var_map break_label continue_label)
        (StatementItem(st)) in
        Buffer.add_string buf (continue_label ^ ":\n");
        generate_update_esp buf condition_var_map;
        (match exp3_opt with
        | None -> ()
        | Some exp -> generate_expression condition_var_map exp);
        Buffer.add_string buf ("jmp    " ^ cond_label ^ "\n");
        Buffer.add_string buf (end_label ^ ":\n");
        generate_update_esp buf condition_var_map;
        (* The variables declared in the block will be deacllocated automatically inside generate_block_item. *)
        (* The condition expressions has its own scope, needs to be deallocated here. *)
        Buffer.add_string buf ("addl $" ^ (string_of_int (4 *  VarMap.cardinal condition_var_map.cur_scope_vars)) ^ ", %esp\n");
        var_map

    | StatementItem(BreakStatement) ->
        if var_map.break_label = "" then raise (CodeGenError "Illegal break, no context.");
        Buffer.add_string buf ("jmp    " ^ var_map.break_label ^ "\n");
        var_map

    | StatementItem(ContinueStatement) ->
        if var_map.continue_label = "" then raise (CodeGenError "Illegal jump, no context.");
        Buffer.add_string buf ("jmp    " ^ var_map.continue_label ^ "\n");
        var_map

    | StatementItem(WhileStatement(exp, st)) ->
        let cond_label = get_unique_label "_whilecond" count in
        let end_label = get_unique_label "_whileend" count in
        let break_label = end_label in
        let continue_label = cond_label in
        Buffer.add_string buf (cond_label ^ ":\n");
        generate_update_esp buf var_map;
        generate_expression var_map exp;
        Buffer.add_string buf ("cmpl    $0, %eax\nje    " ^ end_label ^ "\n");
        generate_block_item (update_break_continue_label var_map break_label continue_label) (StatementItem(st));
        Buffer.add_string buf ("jmp    " ^ cond_label ^ "\n");
        Buffer.add_string buf (end_label ^ ":\n");
        generate_update_esp buf var_map;
        var_map

    | StatementItem(DoStatement(st, exp)) ->
        let begin_label = get_unique_label "_dobegin" count in
        let end_label = get_unique_label "_doend" count in
        let break_label = end_label in
        let continue_label = get_unique_label "_docontinue" count in
        Buffer.add_string buf (begin_label ^ ":\n");
        generate_block_item (update_break_continue_label var_map break_label continue_label) (StatementItem(st));
        Buffer.add_string buf (continue_label ^ ":\n");
        generate_update_esp buf var_map;
        generate_expression var_map exp;
        Buffer.add_string buf ("cmpl    $0, %eax\njne    " ^ begin_label ^ "\n");
        Buffer.add_string buf (end_label ^ ":\n");
        generate_update_esp buf var_map;
        var_map

    | StatementItem (CompoundStatement(items)) ->
        (* Entering a new scope, so clear the cur_scope_vars, but we ignore the inner var_map returned. *)
        let inner_var_map = generate_block_statements
        {vars = var_map.vars;
        cur_scope_vars = VarMap.empty;
        index = var_map.index;
        continue_label = var_map.continue_label;
        break_label = var_map.break_label} items in
        (* Move stack pointer %esp back by number of allocations in the inner scope. This is like deallocating inner variables.*)
        Buffer.add_string buf ("addl $" ^ (string_of_int (4 *  VarMap.cardinal inner_var_map.cur_scope_vars)) ^ ", %esp\n");
        var_map
    | DeclareItem (DeclareStatement (a, exp_opt)) ->
        (match exp_opt with
        | None ->
            Buffer.add_string buf "movl    $0, %eax\n"
        | Some exp ->
            generate_expression var_map exp);
        Buffer.add_string buf "push    %eax\n";
        (match (VarMap.find_opt a var_map.cur_scope_vars) with
        | Some x -> raise (CodeGenError ("Var " ^ a ^ " is already defined in current scope!"))
        | None -> { vars = VarMap.add a var_map.index var_map.vars;
        cur_scope_vars = VarMap.add a var_map.index var_map.cur_scope_vars;
        index = var_map.index - 4;
        break_label = var_map.break_label;
        continue_label = var_map.continue_label})

  and generate_block_statements var_map sts =
    List.fold_left generate_block_item var_map sts
  in
  let generate_function f =
    (* index is the next available offset to esp to save new local variables, at the
     * beginning of a function, the index is one word (4 bytes) after the esp register. *)

  let var_map = {
    vars = VarMap.empty;
    cur_scope_vars = VarMap.empty;
    index =  -4;
    break_label = "";
    continue_label = ""} in
    match f with
    | IntFunction (fname, items) -> 
        Buffer.add_string buf ("_" ^ fname ^ ":\n");
        (* Saving the previous stack start point and use esp as the new stack start. *)
        Buffer.add_string buf "push    %ebp\nmovl    %esp, %ebp\n";
        generate_block_statements var_map items
  in

  Buffer.add_string buf ".globl _main\n";
  (match ast with
  | Program f ->
      generate_function f);
  Buffer.contents buf

let _ =
  let ast = get_ast (parse_tokens (read_file_content "test.cc"))
  in
  Printf.printf "\nParsed AST: \n%s" (print_ast ast);
  let code = generate_assembly ast
  in
  Printf.printf "\nGenerated Code:\n%s\n" code;
  let oc = open_out "assembly.s" in
  Printf.fprintf oc "%s\n" code





