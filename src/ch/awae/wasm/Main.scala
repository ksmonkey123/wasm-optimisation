package ch.awae.wasm

import ch.awae.wasm.ast.NumericValue.I32
import ch.awae.wasm.ast.implicits._
import ch.awae.wasm.ast.{BinaryModule, Compiler}
import ch.awae.wasm.io.DataStream
import ch.awae.wasm.io.implicits._

object Main extends App {

  val x = "change.wasm".file.ast

  val y = Compiler.compile(x)

  Console println BinaryModule(DataStream.ofList(y))

  Console println I32(123457).unsigned

}