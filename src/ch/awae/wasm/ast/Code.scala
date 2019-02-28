package ch.awae.wasm.ast

case class Locals(count: Int, valType: ValueType)
object Locals {
  private[ast] def apply(stream: DataStream): Locals = {
    val num = I32(stream).unsigned
    Locals(num, ValueType(stream))
  }
}

case class Code(locals: List[Locals], body: Expression)
object Code {
  private[ast] def apply(stream: DataStream): Code = {
    I32(stream)
    Code(Vec(stream, Locals(_)).toList, Expression(stream))
  }
}

