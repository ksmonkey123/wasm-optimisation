package ch.awae.wasm

import ch.awae.wasm.io.implicits._
import ch.awae.wasm.ast.implicits._

object Main extends App {

  val module = "change.wasm".file.ast.module
  
  println(module)

}