package ch.awae.wasm

import ch.awae.wasm.ast.Instruction
import ch.awae.wasm.io.ListBackedDataStream
import ch.awae.wasm.ast.Parser

object Main extends App {

  println(Parser.parseFile("math.wasm"));

}