package ch.awae.wasm.ast

case class Locals(count: Int, valType: ValueType)
object Locals {
  private[ast] def apply(stream: DataStream): Locals = Locals(I32(stream).unsigned, ValueType(stream))
}

case class Code(locals: List[Locals], body: Expression)
object Code {
  private[ast] def apply(stream: DataStream): Code = {
    stream.take
    Code(Vec(stream, Locals(_)).toList, Expression(stream))
  }
}

