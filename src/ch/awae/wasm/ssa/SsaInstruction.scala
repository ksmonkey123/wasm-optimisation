package ch.awae.wasm.ssa

import ch.awae.wasm.ast.Instruction

sealed trait SsaInstruction {
  def result: Option[Int]

  def inputs: List[Int]

  def replaceInput(original: Int, replacement: Int): Option[SsaInstruction]
}


object SsaInstruction {

  case class Φ(target: Int, inputs: List[Int]) extends SsaInstruction {
    def result = Some(target)

    override def replaceInput(original: Int, replacement: Int): Some[Φ] = {
      Some(Φ(target, inputs.map({
        case `original` => replacement
        case x => x
      })))
    }
  }

  case class ASSIGN(target: Int, input: Int) extends SsaInstruction {
    def result = Some(target)

    def inputs = List(input)

    override def replaceInput(original: Int, replacement: Int): Option[ASSIGN] = Some(ASSIGN(target, replacement))
  }

  case class OPERATION(inst: Instruction, result: Option[Int], inputs: List[Int]) extends SsaInstruction {
    override def replaceInput(original: Int, replacement: Int): Option[SsaInstruction] = {
      Some(OPERATION(inst, result, inputs.map({
        case `original` => replacement
        case x => x
      })))
    }
  }

}