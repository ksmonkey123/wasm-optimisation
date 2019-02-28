package ch.awae.wasm.ast

object Vec {
  private[ast] def apply[T](stream: DataStream, elemParser: DataStream => T): List[T] = {
    val size = I32(stream).unsigned.toInt
    (for (i <- 1 to size) yield (elemParser(stream))).toList
  }
}