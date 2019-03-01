package ch.awae.wasm.ast

import scala.annotation.tailrec

trait NumericValue

case class I32(bytes: List[Byte]) extends NumericValue {
  def unsigned: Int = {
    bytes.indices.toList
      .map(7 * _)
      .zip(bytes.reverse)
      .map { case (shift, byte) => (byte & 0x7f) << shift }
      .reduce(_ | _)
  }
}

case class I64(bytes: List[Byte]) extends NumericValue

case class F32(bytes: List[Byte]) extends NumericValue

case class F64(bytes: List[Byte]) extends NumericValue

object I32 {
  private[ast] def apply(stream: DataStream): I32 = LSB128(stream, apply)
}

object I64 {
  private[ast] def apply(stream: DataStream): I64 = LSB128(stream, apply)
}

object F32 {
  private[ast] def apply(stream: DataStream): F32 = F32(stream take 4)
}

object F64 {
  private[ast] def apply(stream: DataStream): F64 = F64(stream take 8)
}

private object LSB128 {

  def apply[T](stream: DataStream, f: List[Byte] => T): T = {
    @tailrec
    def collect(acc: List[Byte]): List[Byte] = stream.take match {
      case x if x < 0 => collect(x :: acc)
      case x => (x :: acc).reverse
    }

    f(collect(Nil))
  }

}
