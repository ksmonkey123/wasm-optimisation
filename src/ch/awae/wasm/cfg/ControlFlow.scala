package ch.awae.wasm.cfg

import java.util.UUID

import scala.collection.mutable.ListBuffer

class ControlFlow(private[this] val _blocks : ListBuffer[SimpleBlock] = ListBuffer.empty) {

  var main : UUID = _

  def blocks : List[SimpleBlock] = _blocks.toList

  def += (block: SimpleBlock) : Unit = _blocks += block

  def dot:String =
    "digraph G {\nSTART -> \"" + main + "\"\n" + blocks.map(_.dot).reduce(_ + "\n" + _) + "\n}"

}