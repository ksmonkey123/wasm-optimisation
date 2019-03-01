package ch.awae.wasm.ast

import ch.awae.wasm.ast.NumericValue.I32

object Vec {
  private[ast] def apply[T](stream: DataStream, elemParser: DataStream => T): List[T] = {
    val size = I32(stream).unsigned
    (for (_ <- 1 to size) yield elemParser(stream)).toList
  }
}