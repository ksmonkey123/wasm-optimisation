package ch.awae.wasm.ast


case class Expression(instructions: List[Instruction])

object Expression {
  private[ast] def apply(stream: DataStream): Expression = Expression(Instruction.getInstructionsForBlock(stream)._1)
}