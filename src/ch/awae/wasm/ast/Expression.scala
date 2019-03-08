package ch.awae.wasm.ast


case class Expression(instructions: List[Instruction])

object Expression {
  def apply(stream: DataStream): Expression = Expression(Instruction.getInstructionsForBlock(stream)._1)
}