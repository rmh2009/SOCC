open Lexer
open Parser
open Codegen
open Codegen_util

module CG = MakeCodeGenUtil(System32Bit)

let _ =
  CG.gen_command (Mov(Reg(BP), Reg(SP))) (PointerType(UnknownType)) |> print_endline;
  CG.gen_command (Mov(RegV(BP), Reg(SP))) (PointerType(UnknownType)) |> print_endline;
  CG.gen_command (Mov(RegV(BP), Reg(AX))) (IntType) |> print_endline;
  CG.gen_command (Mov(Disp(8,BP), Reg(BX))) (IntType) |> print_endline;
  CG.gen_command (Mov(Disp(8,BP), Reg(BX))) (PointerType(UnknownType)) |> print_endline;
  CG.gen_command (Mov(Reg(AX), Disp(8, BP))) IntType |> print_endline;
  ()

