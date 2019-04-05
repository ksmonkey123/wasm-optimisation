package ch.awae.wasm.ssa

import java.util.UUID

import ch.awae.wasm.cfg.{AsmPrinter, ControlFlow}
import ch.awae.wasm.ssa.SsaInstruction.{ASSIGN, OPERATION, Φ}

import scala.collection.mutable.ListBuffer

class SsaBlock(
                val uuid: UUID,
                val succ: ListBuffer[UUID] = ListBuffer.empty,
                val inst: ListBuffer[SsaInstruction] = ListBuffer.empty,
                val rawFlow: ControlFlow
              ) {
  def replaceSymbol(original: Int, replacement: Int): Unit = {
    val newInsts = (for {
      instruction <- inst
    } yield {
      if (instruction.inputs contains original) {
        instruction.replaceInput(original, replacement)
      } else if (instruction.result.contains(original)) {
        None
      } else {
        Some(instruction)
      }
    }).flatten
    inst.clear()
    inst.insertAll(0, newInsts)
  }

  def findOneAssignment: Option[ASSIGN] = {
    inst.find(_.isInstanceOf[ASSIGN]).map(_.asInstanceOf[ASSIGN])
  }


  def dotSuccessors: List[String] = succ.toList match {
    case Nil => Nil
    case List(x) => List("\"" + uuid + "\" -> \"" + x + "\"")
    case List(y, n) => List("\"" + uuid + "\" -> \"" + y + "\" [color=green]", "\"" + uuid + "\" -> \"" + n + "\" [color=red]")
    case list => list.zipWithIndex.map {
      case (item, index) => "\"" + uuid + "\" -> \"" + item + "\" [label=" + index + "]"
    }
  }

  def parseInst(instruction: SsaInstruction): String = instruction match {
    case ASSIGN(target, input) => "%04X := %04X".format(target, input)
    case OPERATION(i, Some(target), inputs) => "%04X := OP(".format(target) + inputs.map(i => "%04X".format(i)).reduceOption(_ + ", " + _).getOrElse("") + ") " + AsmPrinter(i, rawFlow.module).substring(5)
    case OPERATION(i, None, inputs) => "OP(" + inputs.map(i => "%04X".format(i)).reduceOption(_ + ", " + _).getOrElse("") + ") " + AsmPrinter(i, rawFlow.module).substring(5)
    case Φ(target, inputs) => "%04X := Φ(".format(target) + inputs.map(i => "%04X".format(i)).reduceOption(_ + ", " + _).getOrElse("") + ")"
  }

  def dot: String = {
    val node = "\"" + uuid + "\" [shape=rectangle, fontname=Monospace, label=\"" + inst.map(parseInst(_) + "\\l").reduceOption(_ + _).getOrElse("") + "\"]"
    dotSuccessors.foldLeft(node)(_ + "\n" + _)
  }

  def replaceUnnecessaryPhisWithAssignment(): Int = {
    (for {
      instruction <- inst
      if instruction.isInstanceOf[Φ]
      φ = instruction.asInstanceOf[Φ]
      if φ.inputs.distinct.size == 1
      index = inst.indexOf(φ)
    } yield {
      inst(index) = ASSIGN(φ.target, φ.inputs.head)
      1
    }).sum
  }

}