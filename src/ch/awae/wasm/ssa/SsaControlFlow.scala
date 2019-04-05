package ch.awae.wasm.ssa

import java.util.UUID

import scala.annotation.tailrec

class SsaControlFlow(val start: UUID, val end: UUID, val blocks: List[SsaBlock]) {
  def dot: String = "digraph G {\nSTART -> \"" + start + "\"\n" + blocks.map(_.dot).reduce(_ + "\n" + _) + "\n\"" + end + "\" -> END\n}"

  def removeUselessPhi(): Boolean = {
    blocks.map(_.replaceUnnecessaryPhisWithAssignment()).sum > 0
  }

  def pruned(): SsaControlFlow = {
    prune()
    this
  }

  @tailrec
  final def prune(): Unit = {
    val n = removeUselessPhi()
    val assignment = blocks.toStream.flatMap(_.findOneAssignment).take(1).toList.headOption
    if (assignment.isDefined) {
      for (block <- blocks)
        block.replaceSymbol(assignment.get.target, assignment.get.input)
    }
    if (n || assignment.isDefined)
      prune()
  }

}