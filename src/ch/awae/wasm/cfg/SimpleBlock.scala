package ch.awae.wasm.cfg

import java.util.UUID

import ch.awae.wasm.ast.Instruction

import scala.collection.mutable.ListBuffer

class SimpleBlock(
                 val controlFlow: ControlFlow,
                 val uuid: UUID = UUID.randomUUID(),
                 val stackframe : Int,
                 private[this] val _instructions : ListBuffer[Instruction] = ListBuffer.empty,
                 var successors : List[UUID] = Nil) {

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
    val node = "\"" + uuid + "\" [shape=rectangle, fontname=Monospace, label=\"" + instructions.map(" " + AsmPrinter(_) + "        \\l").foldLeft(s"   :: $stackframe ::   \\n")(_ + _) + "\"]"
    dotSuccessors.foldLeft(node)(_ + "\n" + _)
  }

  def predecessors : List[UUID] = for {
    block <- controlFlow.blocks
    if block.successors contains uuid
  } yield {
    block.uuid
  }

}

object SimpleBlock {
  def apply(stackframe : Int)(implicit controlFlow: ControlFlow):SimpleBlock = {
    val block = new SimpleBlock(controlFlow, stackframe = stackframe)
    controlFlow += block
    block
  }
}