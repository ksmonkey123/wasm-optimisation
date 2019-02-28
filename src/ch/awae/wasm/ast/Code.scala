package ch.awae.wasm.ast

case class Locals(count: I32, valType: ValueType)
object Locals {
  def apply(stream: DataStream): Locals = Locals(I32(stream), ValueType(stream))
}

case class Code(locals: Vec[Locals], body: Expression)
object Code {
  def apply(stream: DataStream): Code = {
    stream.take
    Code(Vec(stream, Locals(_)), Expression(stream))
  }
}

