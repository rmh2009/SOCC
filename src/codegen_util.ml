open Type
open Typeutil

exception CodeGenUtilError of string
let fail_genutil msg =
  raise (CodeGenUtilError(msg))

let is_64_bit = false
let is_32_bit = not is_64_bit

let rec get_data_size (t : data_type_t) : int =
  match t with
  | IntType -> 4
  | ArrayType (t2, size) -> size * get_data_size t2
  | PointerType _ -> if is_64_bit then 8 else 4
  | _ -> TokenError "Unsupported data type." |> raise

type register_t =
  | BP
  | SP
  | AX
  | BX
  | CX
  | DX

type operand_t =
  | Imm of int  (* $8 *)
  | Reg of register_t  (* such as %rax *)
  | RegV of register_t (* such as (%rax) *)
  | Disp of int * register_t (* 8(%rax) *)
  | Index of int * register_t * register_t * int (* offset(%rcx, %rax, 4) = %rcx + %rax * 4  + offset *)

type command_t = 
  | Mov of operand_t * operand_t
  | Movz of operand_t * operand_t
  | Movs of operand_t * operand_t
  | Comp of operand_t * operand_t
  | Jmp of string
  | Je of string
  | Jne of string
  | Push of operand_t
  | Pop of operand_t
  | Add of operand_t * operand_t
  | Sub of operand_t * operand_t
  | Mul of operand_t * operand_t
  | Div of operand_t
  | Lea of operand_t * operand_t
  | Xor of operand_t

let gen_register (r : register_t) (dtype : data_type_t) =
  let dsize = get_data_size dtype in
  match r with
  | BP -> if is_64_bit then "%rbp" else "%ebp"
  | SP -> if is_64_bit then "%rsp" else "%esp"
  | AX -> if dsize = 8 then "%rap" else "%eax"
  | BX -> if dsize = 8 then "%rbx" else "%ebx"
  | CX -> if dsize = 8 then "%rcx" else "%ecx"
  | DX -> if dsize = 8 then "%rdx" else "%edx"

let gen_operand (dtype : data_type_t)(op : operand_t) =
  match op with
  | Imm a -> "$" ^ (string_of_int a)
  | Reg r -> gen_register r dtype
  | RegV r -> "(" ^ (gen_register r dtype) ^ ")"
  | Disp (offset, r) -> Printf.sprintf "%d(%s)" offset (gen_register r dtype)
  | Index (offset, base, index, step) -> Printf.sprintf "%d(%s, %s, %d)" offset (gen_register base dtype) (gen_register index dtype) step

let gen_byte_suffix dtype =
  let dsize = get_data_size dtype in
  if dsize = 8 then "q"
  else if dsize = 4 then "l"
  else if dsize = 2 then "w"
  else if dsize = 1 then "b"
  else "Illegal data size: " ^ string_of_int dsize |> fail_genutil

let gen_command (command : command_t) dtype =
  let genb = gen_byte_suffix dtype in
  let geno = gen_operand dtype in
  let gen_two c a b =
    [ c ^ genb; geno a; geno b] in
  let gen_one c a =
    [ c ^ genb; geno a] in
  match command with
  | Mov(a,b) -> gen_two "mov" a b 
  | Movz(a,b) -> fail_genutil "movz not supported yet."
  | Movs(a,b) -> fail_genutil "movs not supported yet."
  | Comp(a,b) -> gen_two "comp" a b 
  | Jmp(s) -> ["jmp"; s]
  | Jne(s) -> ["jne"; s]
  | Je(s) -> ["je"; s]
  | Push(a) -> gen_one "push" a
  | Pop(a) -> gen_one "pop" a
  | Add(a,b) -> gen_two "add" a b 
  | Sub(a,b) -> gen_two "sub" a b 
  | Mul(a,b) -> gen_two "imul" a b 
  | Div(a) -> gen_one "idiv" a
  | Lea(a,b) -> gen_two "lea" a b
  | Xor(a) -> gen_one "xor" a

let format_commands str_list =
  if List.length str_list = 3 then
    Printf.sprintf "%s    %s,%s\n" (List.nth str_list 0) (List.nth str_list 1)(List.nth str_list 2)
  else if List.length str_list = 2 then
    Printf.sprintf "%s    %s\n" (List.nth str_list 0)(List.nth str_list 1)
  else
    fail_genutil "expecting 2 or 3 operands."


