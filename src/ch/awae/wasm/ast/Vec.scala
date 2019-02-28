package ch.awae.wasm.ast

case class Vec[T](elems: List[T])

object Vec {
  def apply[T](stream: DataStream, elemParser: DataStream => T): Vec[T] = {
    val size = I32(stream).unsigned.toInt
    Vec((for (i <- 1 to size) yield (elemParser(stream))).toList)
  }
}