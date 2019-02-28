package ch.awae.wasm.ast

trait ResultType
trait ValueType extends ResultType

object ValueType {
  case object i32 extends ValueType
  case object i64 extends ValueType
  case object f32 extends ValueType
  case object f64 extends ValueType

  private[ast] def apply(stream: DataStream): ValueType = stream.take match {
    case 0x7f => i32
    case 0x7e => i64
    case 0x7d => f32
    case 0x7c => f64
  }
}

object ResultType {
  case object void extends ResultType

  private[ast] def apply(stream: DataStream) = stream.take match {
    case 0x40 => void
    case x    => ValueType(x)
  }
}

case class FunctionType(paramTypes: List[ValueType], returnType: Option[ValueType])

object FunctionType {
  private[ast] def apply(stream: DataStream): FunctionType = stream.take match {
    case 0x60 => FunctionType(Vec(stream, ValueType(_)), Vec(stream, ValueType(_)).headOption)
  }
}

sealed trait Limit

case class MinLimit(min: Int) extends Limit
case class MinMaxLimit(min: Int, max: Int) extends Limit

object Limit {
  private[ast] def apply(stream: DataStream) = stream.take match {
    case 0x00 => MinLimit(I32(stream).unsigned)
    case 0x01 => MinMaxLimit(I32(stream).unsigned, I32(stream).unsigned)
  }
}

case class MemoryType(limits: Limit)
object MemoryType {
  private[ast] def apply(stream: DataStream): MemoryType = MemoryType(Limit(stream))
}

case class TableType(limits: Limit)
object TableType {
  private[ast] def apply(stream: DataStream): TableType = stream.take match {
    case 0x70 => TableType(Limit(stream))
  }
}

case class GlobalType(valType: ValueType, isFinal: Boolean)
object GlobalType {
  private[ast] def apply(stream: DataStream): GlobalType = {
    val valType = ValueType(stream)
    stream.take match {
      case 0x00 => GlobalType(valType, true)
      case 0x01 => GlobalType(valType, false)
    }
  }
}
