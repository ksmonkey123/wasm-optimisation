package ch.awae.wasm.ast

import scala.annotation.tailrec

trait Instruction { def hasSideEffects = false }

trait ControlInstruction extends Instruction
trait BlockInstruction extends ControlInstruction { def blockType: ResultType }
trait ParametricInstruction extends Instruction
trait VariableInstruction extends Instruction
trait MemoryInstruction extends Instruction
trait NumericInstruction extends Instruction

object Instruction {
  case object UNREACHABLE extends ControlInstruction
  case object NOP extends ControlInstruction
  case class BLOCK(blockType: ResultType, instructions: List[Instruction]) extends BlockInstruction
  case class LOOP(blockType: ResultType, instructions: List[Instruction]) extends BlockInstruction
  case class IFELSE(blockType: ResultType, ifBlock: List[Instruction], elseBlock: List[Instruction]) extends BlockInstruction
  case class BREAK(label: Int) extends ControlInstruction
  case class BREAK_COND(label: Int) extends ControlInstruction
  case class BREAK_TABLE(table: List[Int], default: Int) extends ControlInstruction
  case object RETURN extends ControlInstruction
  case class CALL(funcId: Int) extends ControlInstruction
  case class CALL_INDIRECT(typeId: Int) extends ControlInstruction

  case object DROP extends ParametricInstruction
  case object SELECT extends ParametricInstruction

  case class LOCAL_GET(id: Int) extends VariableInstruction
  case class LOCAL_SET(id: Int) extends VariableInstruction
  case class LOCAL_TEE(id: Int) extends VariableInstruction
  case class GLOBAL_GET(id: Int) extends VariableInstruction
  case class GLOBAL_SET(id: Int) extends VariableInstruction

  case class MEMARG_INSTRUCTION(inst: Byte, a: Int, o: Int) extends MemoryInstruction
  case object MEMORY_SIZE extends MemoryInstruction
  case object MEMORY_GROW extends MemoryInstruction

  case class CONST_INSTRUCTION(inst: Byte, value: NumericValue) extends NumericInstruction
  case class PLAIN_NUMERIC_INSTRUCTION(inst: Byte) extends NumericInstruction

  // FACTORY
  private val memarg_instructions: List[Byte] = (0x28 to 0x3e).toList.map(_.toByte)
  private def isMemArg(b: Byte) = memarg_instructions contains b
  private def isNumeric(b: Byte) = (b & 0x000000ff) >= 0x00000045 && b <= 0x000000bf

  private[ast] def apply(stream: DataStream): Instruction = stream.take match {
    // control instructions
    case 0x00 => UNREACHABLE
    case 0x01 => NOP
    case 0x02 => BLOCK(ResultType(stream), getInstructionsForBlock(stream)._1)
    case 0x03 => LOOP(ResultType(stream), getInstructionsForBlock(stream)._1)
    case 0x04 => {
      val resultType = ResultType(stream)
      val (ifBlock, firstTerminator) = getInstructionsForBlock(stream)
      IFELSE(resultType, ifBlock, if (firstTerminator == 0x05) getInstructionsForBlock(stream)._1 else Nil)
    }
    case 0x0c              => BREAK(I32(stream).unsigned)
    case 0x0d              => BREAK_COND(I32(stream).unsigned)
    case 0x0e              => BREAK_TABLE(Vec(stream, I32(_).unsigned), I32(stream).unsigned)
    case 0x0f              => RETURN
    case 0x10              => CALL(I32(stream).unsigned)
    case 0x11              => CALL_INDIRECT(I32(stream).unsigned) afterVerify { stream.take == 0x00 }
    // parametric instructions
    case 0x1a              => DROP
    case 0x1b              => SELECT
    // variable instructions
    case 0x20              => LOCAL_GET(I32(stream).unsigned)
    case 0x21              => LOCAL_SET(I32(stream).unsigned)
    case 0x22              => LOCAL_TEE(I32(stream).unsigned)
    case 0x23              => GLOBAL_GET(I32(stream).unsigned)
    case 0x24              => GLOBAL_SET(I32(stream).unsigned)
    // memory instructions
    case x if isMemArg(x)  => MEMARG_INSTRUCTION(x, I32(stream).unsigned, I32(stream).unsigned)
    case 0x3f              => MEMORY_SIZE afterVerify { stream.take == 0x00 }
    case 0x40              => MEMORY_GROW afterVerify { stream.take == 0x00 }
    // numeric instructions
    case 0x41              => CONST_INSTRUCTION(0x41, I32(stream))
    case 0x42              => CONST_INSTRUCTION(0x42, I64(stream))
    case 0x43              => CONST_INSTRUCTION(0x43, F32(stream))
    case 0x44              => CONST_INSTRUCTION(0x44, F64(stream))
    case x if isNumeric(x) => PLAIN_NUMERIC_INSTRUCTION(x)
  }

  @tailrec
  private[ast] def getInstructionsForBlock(stream: DataStream, acc: List[Instruction] = Nil): (List[Instruction], Byte) = stream.take match {
    case x @ (0x05 | 0x0b) => (acc.reverse, x)
    case x                 => getInstructionsForBlock(stream, Instruction(x :: stream) :: acc)
  }

}

case class Expression(instructions: List[Instruction])
object Expression {
  private[ast] def apply(stream: DataStream): Expression = Expression(Instruction.getInstructionsForBlock(stream)._1)
}