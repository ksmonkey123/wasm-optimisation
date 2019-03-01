package ch.awae.wasm.ast

import ch.awae.wasm.ast.NumericValue.I32
import ch.awae.wasm.ast.Types.ValueType


case class Code(locals: List[Code.Locals], body: Expression)

object Code {
  private[ast] def apply(stream: DataStream): Code = {
    I32(stream)
    Code(Vec(stream, Locals(_)), Expression(stream))
  }

  case class Locals(count: Int, valType: ValueType)

  object Locals {
    private[ast] def apply(stream: DataStream): Locals = {
      val num = I32(stream).unsigned
      Locals(num, ValueType(stream))
    }
  }
}

