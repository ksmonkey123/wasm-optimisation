package ch.awae.wasm

import ch.awae.wasm.ast.WasmFunction.DeclaredFunction
import ch.awae.wasm.ast.implicits._
import ch.awae.wasm.io.implicits._
import ch.awae.wasm.ssa.SsaParser
import ch.awae.wasm.util.Dot

object Main extends App {

  //  7 - very long
  // 10 - complex
  // 15 - nop
  // 16 - simple nested branching
  // 17 - return 0
  // 18 - deep nesting
  // 20 - massive
  // 21 - slightly less massive

  val module = "change.wasm".file.ast.module
  val functions = module.funcs.filter(_.isInstanceOf[DeclaredFunction]).map(_.asInstanceOf[DeclaredFunction])

  val f = functions(16)

  val flow = cfg.Builder.build(f, module)
  Dot(flow.dot, "cfg-raw")

  flow.prune()
  Dot(flow.dot, "cfg-min")

  val ssa = new SsaParser(flow).parse()
  Dot(ssa.dot, "ssa-raw")

  ssa.prune()
  Dot(ssa.dot, "ssa-min")
}
