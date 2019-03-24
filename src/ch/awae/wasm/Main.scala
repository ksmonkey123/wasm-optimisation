package ch.awae.wasm

import ch.awae.wasm.ast.Instruction._
import ch.awae.wasm.ast.Types.ValueType.f64
import ch.awae.wasm.ast.WasmFunction.DeclaredFunction

object Main extends App {

  val f = DeclaredFunction(0,List(), List(
    LOCAL_GET(0),
    LOCAL_GET(0),
    BLOCK(f64,
      IFELSE(f64,
        PLAIN_NUMERIC_INSTRUCTION(108)::Nil,
        BREAK_COND(1) :: UNREACHABLE ::Nil) :: Nil),
    RETURN,
    UNREACHABLE))

  val flow = cfg.Builder.build(f)

  println(flow.dot)

}