package ch.awae.wasm

import ch.awae.wasm.ast.BinaryModule
import ch.awae.wasm.ast.implicits._
import ch.awae.wasm.io.DataStream
import ch.awae.wasm.io.implicits._

object Main extends App {

  val module = "change.wasm".file.ast

  println(module)

  val bin = ast.Compiler.compile(module)

  val mod2 = BinaryModule(DataStream.ofList(bin))

  println(mod2)

}