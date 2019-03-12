package ch.awae.wasm

import ch.awae.wasm.ast.implicits._
import ch.awae.wasm.ast.{BinaryModule, Compiler}
import ch.awae.wasm.io.DataStream
import ch.awae.wasm.io.implicits._

object Main extends App {

  val module = "change.wasm".file.ast.module

  val mod2 = BinaryModule(DataStream.ofList(Compiler.compile(module.ast))).module

  Console println (module == mod2)

  Compiler.compile(module.ast).write("out.wasm")

}