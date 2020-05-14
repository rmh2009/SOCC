open Lexer
open Parser
open Codegen
open Codegen_util

let _ =
  format_commands (gen_command (Mov(Reg(BP), Reg(SP))) (PointerType(UnknownType))) |> print_endline;
  format_commands (gen_command (Mov(RegV(BP), Reg(SP))) (PointerType(UnknownType))) |> print_endline;
  format_commands (gen_command (Mov(RegV(BP), Reg(AX))) (IntType)) |> print_endline;
  format_commands (gen_command (Mov(Disp(8,BP), Reg(BX))) (IntType)) |> print_endline;
  format_commands (gen_command (Mov(Disp(8,BP), Reg(BX))) (PointerType(UnknownType))) |> print_endline;
  ()

