package ch.awae.wasm.cfg

import java.util.UUID

import scala.collection.mutable.ListBuffer

class ControlFlow(private[this] val _blocks : ListBuffer[SimpleBlock] = ListBuffer.empty) {

  var start : UUID = _
  var end : UUID = _

  def blocks : List[SimpleBlock] = _blocks.toList

  def += (block: SimpleBlock) : Unit = _blocks += block

  def dot:String =
    "digraph G {\nSTART -> \"" + start + "\"\n" + blocks.map(_.dot).reduce(_ + "\n" + _) + "\n\"" + end + "\" -> END\n}"

}