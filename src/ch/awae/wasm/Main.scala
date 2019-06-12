package ch.awae.wasm

import scala.language.postfixOps

object Main extends App {

  //  7 - very long
  // 10 - complex
  // 15 - nop
  // 16 - simple nested branching
  // 17 - return 0
  // 18 - deep nesting
  // 20 - massive
  // 21 - slightly less massive

  val module = load module "change.wasm"
  module.module.functions foreach println

  module process 17

}
