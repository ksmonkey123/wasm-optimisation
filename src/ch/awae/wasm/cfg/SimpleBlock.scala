package ch.awae.wasm.cfg

import java.util.UUID

import ch.awae.wasm.ast.Instruction

import scala.collection.mutable.ListBuffer

class SimpleBlock(
                 val uuid: UUID = UUID.randomUUID(),
                 private[this] val _instructions : ListBuffer[Instruction] = ListBuffer.empty,
                 var successors : List[UUID] = Nil
                 ) {

  def instructions: List[Instruction] = _instructions.toList

  def +=(instruction: Instruction): Unit = _instructions += instruction

  def dotSuccessors:List[String] = successors match {
    case Nil => Nil
    case List(x) => List("\"" + uuid + "\" -> \"" + x + "\"")
    case List(y, n) => List("\"" + uuid + "\" -> \"" + y + "\" [color=green]", "\"" + uuid + "\" -> \"" + n + "\" [color=red]")
    case list => list.zipWithIndex.map{
      case (item, index) => "\"" + uuid + "\" -> \"" + item + "\" [label=" + index + "]"
    }
  }

  def dot : String = {
    val node = "\"" + uuid + "\" [shape=box, label=\"" + instructions.map(_.getClass.getSimpleName).reduceOption(_ + "\\n" + _).getOrElse("") + "\"]"
    dotSuccessors.foldLeft(node)(_+"\n" +_)
  }



}

object SimpleBlock {
  def apply()(implicit controlFlow: ControlFlow):SimpleBlock = {
    val block = new SimpleBlock
    controlFlow += block
    block
  }
}