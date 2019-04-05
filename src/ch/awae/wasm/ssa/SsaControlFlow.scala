package ch.awae.wasm.ssa

import java.util.UUID

import ch.awae.wasm.cfg.ControlFlow
import ch.awae.wasm.ssa.SsaInstruction.Φ

import scala.annotation.tailrec

class SsaControlFlow(val start: UUID, val end: UUID, val blocks: List[SsaBlock], val rawFlow: ControlFlow) {
  def dot: String = {
    val label = rawFlow.typ.paramTypes.indices.map("%04X".format(_)).reduceOption(_+","+_).getOrElse("")
    "digraph G {\nSTART [label=\"START\\n(" + label + ")\"]\nSTART -> \"" + start + "\"\n" + blocks.map(_.dot).reduce(_ + "\n" + _) + "\n\"" + end + "\" -> END\n}"
  }

  def removeUselessPhi(): Boolean = {
    blocks.map(_.replaceUnnecessaryPhisWithAssignment()).sum > 0
  }

  def pruned(): SsaControlFlow = {
    prune()
    this
  }

  def findUsages(target: Int): List[SsaInstruction] = blocks.flatMap(_.inst).filter(_.inputs.contains(target))

  def findDeadPhis(): List[Φ] = blocks
    .flatMap(block => block.inst)
    .filter(_.isInstanceOf[Φ])
    .map(_.asInstanceOf[Φ])
    .filter(φ => findUsages(φ.target).isEmpty)

  @tailrec
  final def prune(): Unit = {
    val n = removeUselessPhi()
    val assignment = blocks.toStream.flatMap(_.findOneAssignment).take(1).toList.headOption
    if (assignment.isDefined) {
      for (block <- blocks)
        block.replaceSymbol(assignment.get.target, assignment.get.input)
    }
    val deadPhis = findDeadPhis()
    for {
      φ <- deadPhis
      block <- blocks.find(_.inst.contains(φ))
    } {
      block.removePhi(φ)
    }
    if (n || assignment.isDefined || deadPhis.nonEmpty)
      prune()
  }

}