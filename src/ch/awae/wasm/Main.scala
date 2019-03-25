package ch.awae.wasm

import ch.awae.wasm.ast.WasmFunction.DeclaredFunction
import ch.awae.wasm.io.implicits._
import ch.awae.wasm.ast.implicits._
import ch.awae.wasm.util.Dot

object Main extends App {

  val module = "change.wasm".file.ast.module
  val functions = module.funcs.filter(_.isInstanceOf[DeclaredFunction]).map(_.asInstanceOf[DeclaredFunction])

  //  7 - very long
  // 10 - complex
  // 16 - simple nested branching
  // 18 - deep nesting
  // 20 - huuuge

  val f = functions apply 10

  val flow = cfg.Builder.build(f, module)

  flow.prune()

  println(f)

  Dot(flow.dot)

}