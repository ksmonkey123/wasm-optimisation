package ch.awae.wasm.cfg

import java.util.UUID

import ch.awae.wasm.ast.Instruction.IFELSE
import ch.awae.wasm.ast.Module

import scala.collection.mutable.ListBuffer

class ControlFlow(val module: Module, private[this] var _blocks : ListBuffer[SimpleBlock] = ListBuffer.empty) {

  var start : UUID = _
  var end : UUID = _

  def block(uuid : UUID): SimpleBlock = _blocks.find(_.uuid == uuid).get

  def blocks : List[SimpleBlock] = _blocks.toList

  def += (block: SimpleBlock) : Unit = _blocks += block

  def dot:String = "digraph G {\nSTART -> \"" + start + "\"\n" + blocks.map(_.dot).reduce(_ + "\n" + _) + "\n\"" + end + "\" -> END\n}"

  def prune() : Unit = {
    removeUnreachableNodes()
    //removeEmptyNodes()
    mergeIfElseIntoPredecessor()
  }

  def pruned(): ControlFlow = {
    prune()
    this
  }

  def removeUnreachableNodes() : Unit = {
    val size = _blocks.size
    val unreachables = _blocks.toList.filterNot(b => _blocks.toList.flatMap(_.successors).distinct.contains(b.uuid) || b.uuid == start)
    for (b <- unreachables) {
      _blocks.remove(_blocks.indexOf(b))
    }
    if (size > _blocks.size)
      removeUnreachableNodes()
  }

  def removeEmptyNodes() : Unit = {
    val size = _blocks.size
    val empties = _blocks.toList.filter(b => b.instructions.isEmpty && b.successors.length == 1)
    for (b <- empties) {
      if (b.uuid == start)
        start = b.successors.head
      substitute(b.uuid, b.successors.head)
      _blocks.remove(_blocks.indexOf(b))
    }
  }

  def substitute(original : UUID, replacement : UUID): Unit = {
    for {
      b <- _blocks
      if b.successors contains original
    } {
      b.successors = for {
        s <- b.successors
      } yield {
        if (s == original)
          replacement
        else s
      }
    }
  }

  def isIfElseBlockWithSinglePredecessor(b: SimpleBlock): Boolean = b.instructions match {
    case IFELSE(_, _, _) :: xs => b.predecessors.length == 1 && b.uuid != start
    case _ => false
  }

  def mergeIfElseIntoPredecessor(): Unit = for {
    block <- _blocks
    if isIfElseBlockWithSinglePredecessor(block)
  } {
    val predec = blocks.find(_.uuid == block.predecessors.head).get
    predec += block.instructions.head
    predec.successors = block.successors
    _blocks.remove(_blocks.indexOf(block))
  }

}