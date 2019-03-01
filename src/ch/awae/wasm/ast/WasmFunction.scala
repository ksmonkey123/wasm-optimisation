package ch.awae.wasm.ast

trait WasmFunction {
  def typeIdx: Int
}

case class ImportedFunction(typeIdx: Int, mod: String, name: String) extends WasmFunction

case class DeclaredFunction(typeIdx: Int, locals: List[Locals], body: List[Instruction]) extends WasmFunction