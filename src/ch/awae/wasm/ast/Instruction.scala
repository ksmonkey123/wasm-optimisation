package ch.awae.wasm.ast

import ch.awae.wasm.ast.NumericValue._
import ch.awae.wasm.ast.Types.ResultType

import scala.annotation.tailrec

trait Instruction {
  def countInstructions: (Int, Int)
}

object Instruction {

  object NumericInstruction {
    def testops: List[Byte] = List(0x45, 0x50).map(_.toByte)

    def unops: List[Byte] = List(0x67, 0x68, 0x69, 0x79, 0x7a, 0x7b, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f, 0x90, 0x91, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f).map(_.toByte)

    def relops: List[Byte] = List(0x46, 0x47, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66).map(_.toByte)

    def binops: List[Byte] = List(
      0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
      0x7c, 0x7d, 0x7e, 0x7f, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a,
      0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98,
      0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6
    ).map(_.toByte)

    def convops: List[Byte] = (0xa7 to 0xbf).toList.map(_.toByte)

  }

  trait ControlInstruction extends Instruction

  trait BranchInstruction extends ControlInstruction

  trait BlockInstruction extends ControlInstruction {
    def blockType: ResultType
  }

  trait ParametricInstruction extends Instruction

  abstract class VariableInstruction(val inst: Byte) extends Instruction {
    def id: Int
  }

  trait MemoryInstruction extends Instruction {
    override def countInstructions: (Int, Int) = (1, 0)
  }

  trait NumericInstruction extends Instruction {
    override def countInstructions: (Int, Int) = (1, 0)
  }

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
    case 0x04 =>
      val resultType = ResultType(stream)
      val (ifBlock, firstTerminator) = getInstructionsForBlock(stream)
      IFELSE(resultType, ifBlock, if (firstTerminator == 0x05) getInstructionsForBlock(stream)._1 else Nil)
    case 0x0c => BRANCH(I32(stream).unsigned)
    case 0x0d => BRANCH_COND(I32(stream).unsigned)
    case 0x0e => BRANCH_TABLE(Vec(stream, I32(_).unsigned), I32(stream).unsigned)
    case 0x0f => RETURN
    case 0x10 => CALL(I32(stream).unsigned)
    case 0x11 => CALL_INDIRECT(I32(stream).unsigned) afterVerify {
      stream.take == 0x00
    }
    // parametric instructions
    case 0x1a => DROP
    case 0x1b => SELECT
    // variable instructions
    case 0x20 => LOCAL_GET(I32(stream).unsigned)
    case 0x21 => LOCAL_SET(I32(stream).unsigned)
    case 0x22 => LOCAL_TEE(I32(stream).unsigned)
    case 0x23 => GLOBAL_GET(I32(stream).unsigned)
    case 0x24 => GLOBAL_SET(I32(stream).unsigned)
    // memory instructions
    case x if isMemArg(x) => MEMARG_INSTRUCTION(x, I32(stream).unsigned, I32(stream).unsigned)
    case 0x3f => MEMORY_SIZE afterVerify {
      stream.take == 0x00
    }
    case 0x40 => MEMORY_GROW afterVerify {
      stream.take == 0x00
    }
    // numeric instructions
    case 0x41 => CONST_INSTRUCTION(0x41, I32(stream))
    case 0x42 => CONST_INSTRUCTION(0x42, I64(stream))
    case 0x43 => CONST_INSTRUCTION(0x43, F32(stream))
    case 0x44 => CONST_INSTRUCTION(0x44, F64(stream))
    case x if isNumeric(x) => ARITHMETIC_INSTRUCTION(x)
  }

  @tailrec
  private[ast] def getInstructionsForBlock(stream: DataStream, acc: List[Instruction] = Nil): (List[Instruction], Byte) = stream.take match {
    case x@(0x05 | 0x0b) => (acc.reverse, x)
    case x => getInstructionsForBlock(stream, Instruction(x :: stream) :: acc)
  }

  case class BLOCK(blockType: ResultType, instructions: List[Instruction]) extends BlockInstruction {
    override def countInstructions: (Int, Int) = {
      instructions.map(_.countInstructions).foldLeft((1, 0)) { case ((a, b), (c, d)) =>
        (a + c, b + d)
      }
    }
  }

  case class LOOP(blockType: ResultType, instructions: List[Instruction]) extends BlockInstruction {
    override def countInstructions: (Int, Int) = {
      instructions.map(_.countInstructions).foldLeft((1, 0)) { case ((a, b), (c, d)) =>
        (a + c, b + d)
      }
    }
  }

  case class IFELSE(blockType: ResultType, ifBlock: List[Instruction], elseBlock: List[Instruction]) extends BlockInstruction {
    override def countInstructions: (Int, Int) = {
      (ifBlock ::: elseBlock).map(_.countInstructions).foldLeft((1, 0)) { case ((a, b), (c, d)) =>
        (a + c, b + d)
      }
    }
  }

  case class BRANCH(label: Int) extends BranchInstruction {
    override def countInstructions: (Int, Int) = (1, 0)
  }

  case class BRANCH_COND(label: Int) extends BranchInstruction {
    override def countInstructions: (Int, Int) = (1, 0)
  }

  case class BRANCH_TABLE(table: List[Int], default: Int) extends BranchInstruction {
    override def countInstructions: (Int, Int) = (1, 0)
  }

  case class CALL(funcId: Int) extends ControlInstruction {
    override def countInstructions: (Int, Int) = (1, 0)
  }

  case class CALL_INDIRECT(typeId: Int) extends ControlInstruction {
    override def countInstructions: (Int, Int) = (1, 0)
  }

  case class LOCAL_GET(id: Int) extends VariableInstruction(0x20) {
    override def countInstructions: (Int, Int) = (1, 1)
  }

  case class LOCAL_SET(id: Int) extends VariableInstruction(0x21) {
    override def countInstructions: (Int, Int) = (1, 1)
  }

  case class LOCAL_TEE(id: Int) extends VariableInstruction(0x22) {
    override def countInstructions: (Int, Int) = (1, 1)
  }

  case class GLOBAL_GET(id: Int) extends VariableInstruction(0x23) {
    override def countInstructions: (Int, Int) = (1, 0)
  }

  case class GLOBAL_SET(id: Int) extends VariableInstruction(0x24) {
    override def countInstructions: (Int, Int) = (1, 0)
  }

  case class MEMARG_INSTRUCTION(inst: Byte, a: Int, o: Int) extends MemoryInstruction

  case class CONST_INSTRUCTION(inst: Byte, value: NumericValue) extends NumericInstruction

  case class ARITHMETIC_INSTRUCTION(inst: Byte) extends NumericInstruction

  case object UNREACHABLE extends ControlInstruction {
    override def countInstructions: (Int, Int) = (1, 0)
  }

  case object NOP extends ControlInstruction {
    override def countInstructions: (Int, Int) = (1, 0)
  }

  case object RETURN extends BranchInstruction {
    override def countInstructions: (Int, Int) = (1, 0)
  }

  case object DROP extends ParametricInstruction {
    override def countInstructions: (Int, Int) = (1, 0)
  }

  case object SELECT extends ParametricInstruction {
    override def countInstructions: (Int, Int) = (1, 0)
  }

  case object MEMORY_SIZE extends MemoryInstruction

  case object MEMORY_GROW extends MemoryInstruction

}
