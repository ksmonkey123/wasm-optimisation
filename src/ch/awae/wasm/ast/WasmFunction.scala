package ch.awae.wasm.ast

import ch.awae.wasm.ast.Code.Locals

trait WasmFunction {
  def typeIdx: Int
}

object WasmFunction {
case class ImportedFunction(typeIdx: Int, mod: String, name: String) extends WasmFunction

case class DeclaredFunction(typeIdx: Int, locals: List[Locals], body: List[Instruction]) extends WasmFunction

}