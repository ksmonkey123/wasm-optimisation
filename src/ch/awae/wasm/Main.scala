package ch.awae.wasm

import ch.awae.wasm.ast.Instruction._
import ch.awae.wasm.ast.Types.ValueType.i32
import ch.awae.wasm.ast.WasmFunction.DeclaredFunction
import ch.awae.wasm.io.implicits._
import ch.awae.wasm.ast.implicits._

object Main extends App {

  val functions = "change.wasm".file.ast.module.funcs.filter(_.isInstanceOf[DeclaredFunction]).map(_.asInstanceOf[DeclaredFunction])

  //  7 - very long
  // 10 - complex
  // 16 - simple nested branching
  // 18 - deep nesting
  // 20 - huuuge

  val f = functions apply 10

  //val body = List(LOOP(i32, List(IFELSE(i32, List(NOP), List(NOP)), BRANCH_COND(0))))
  //val f = DeclaredFunction(0, Nil, body)

  val flow = cfg.Builder.build(f)

  flow.prune()

  println(f)

  println(flow.dot)

}