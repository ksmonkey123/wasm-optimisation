package ch.awae.wasm.cfg

import java.util.UUID

import ch.awae.wasm.ast.Instruction
import ch.awae.wasm.ast.Types.ResultType

import scala.collection.mutable.ListBuffer

class SimpleBlock(
                   val controlFlow: ControlFlow,
                   val uuid: UUID = UUID.randomUUID(),
                   val stackframe: Int,
                   var entryType: Option[ResultType] = None,
                   val loopHead: Boolean = false,
                   private[this] val _instructions: ListBuffer[Instruction] = ListBuffer.empty,
                   var successors: List[UUID] = Nil,
                   var stackPredecessor: Option[UUID] = None) {

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

  def dotStackPredecessors: List[String] = (stackPredecessor map { p =>
    "\"" + uuid + "\" -> \"" + p + "\" [style=dashed]"
  }).toList

  def typeSignatureIfNeeded:String = {
    if (predecessors.isEmpty) {
      ""
    }else {
      if (predecessors.size > 1 || predecessors.map(controlFlow.block(_).stackframe).max > this.stackframe)
      s"(${entryType.getOrElse("void")})\\n"
      else
      ""
    }
  }

  def dot : String = {
    val headSymbol = if (loopHead) "##" else "::"
    val node = "\"" + uuid + "\" [shape=rectangle, fontname=Monospace, label=\"" + instructions.map(AsmPrinter(_, controlFlow.module) + "\\l").foldLeft(s"$headSymbol $stackframe $headSymbol\\n$typeSignatureIfNeeded")(_ + _) + "\"]"
    val n2 = dotSuccessors.foldLeft(node)(_ + "\n" + _)
    //dotStackPredecessors.foldLeft(n2)(_ + "\n" + _)
    n2
  }

  def predecessors : List[UUID] = for {
    block <- controlFlow.blocks
    if block.successors contains uuid
  } yield {
    block.uuid
  }

  def takesEntryParam: Boolean = !entryType.forall(ResultType.VOID == _)

}

object SimpleBlock {
  def apply(stackframe: Int, loopHead: Boolean = false, stackPredecessor: UUID = null)(implicit controlFlow: ControlFlow): SimpleBlock = {
    val block = new SimpleBlock(controlFlow, stackframe = stackframe, loopHead = loopHead, stackPredecessor = Option(stackPredecessor))
    controlFlow += block
    block
  }
}