package ch.awae.wasm.ssa

import java.util.UUID

import ch.awae.wasm.ast.Instruction
import ch.awae.wasm.ast.Instruction._
import ch.awae.wasm.cfg.{ControlFlow, SimpleBlock}
import ch.awae.wasm.ssa.SsaInstruction._
import ch.awae.wasm.ssa.SsaParser.ExitState
import ch.awae.wasm.util.{AStack, SequentialIndexer}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class SsaParser(flow: ControlFlow) {

  val indexer = new SequentialIndexer(0)
  val exitMap = mutable.HashMap.empty[UUID, ExitState]
  val desiredLocals = mutable.HashMap.empty[UUID, List[Int]]
  val ssaBlocks = mutable.HashMap.empty[UUID, SsaBlock]

  def initializeLocals(): ListBuffer[Int] = {
    val buffer = ListBuffer.empty[Int]
    for (_ <- 1 to (flow.locals.map(_.count).sum + flow.typ.paramTypes.size)) {
      buffer += indexer.next
    }
    buffer
  }

  /**
    * get the locals at the start of the block as well as any potentially necessary φ-statements
    */
  def getLocalsAtStart(block: SimpleBlock): (ListBuffer[Int], List[SsaInstruction]) = {
    val preds = block.predecessors
    if (preds.isEmpty) {
      // there are no predecessors => create a new set of locals
      (initializeLocals(), Nil)
    } else {
      // there are predecessors => take their locals where possible
      val list = preds.map(b => {
        if (exitMap.contains(b)) {
          // this predecessor is processed => reuse those
          exitMap(b).locals
        } else {
          // this predecessor is not yet processed => create new set of locals
          //    and tell this node to link them later on
          val locals = initializeLocals().toList
          desiredLocals(b) = locals
          locals
        }
      })
      if (list.size == 1) {
        // only one set of predecessor locals => reuse
        (ListBuffer(list.head: _*), Nil)
      } else {
        // multiple predecessor locals => link through a φ
        val locs = initializeLocals()
        val phis = for (i <- locs.indices) yield {
          Φ(locs(i), list.map(_ (i)))
        }
        (locs, phis.toList)
      }
    }
  }

  /**
    * get the stack at the start of the block as well as any potentially necessary φ-statement
    */
  def getStackAtStart(block: SimpleBlock): (List[Int], Option[SsaInstruction]) = {
    val stack = block.stackPredecessor.map(exitMap(_).stack).getOrElse(Nil)

    val preds = block.predecessors

    if (block.takesEntryParam) {
      if (preds.size == 1) {
        (exitMap(preds.head).stack.head :: stack, None)
      } else {
        // this block takes params from multiple predecessors. Therefore all predecessors must provide a value
        val newSymbol = indexer.next
        val φ = Φ(newSymbol, for {
          pred <- preds
          map = exitMap(pred)
          predStack = map.stack
        } yield {
          predStack.head
        })
        // assemble stack and return
        (newSymbol :: stack, Some(φ))
      }
    } else {
      // the block takes no params. It is trivial
      (stack, None)
    }

  }

  def parse(): SsaControlFlow = {
    for (uuid <- flow.topologicalSequence)
      processBlock(uuid)
    new SsaControlFlow(flow.start, flow.end, ssaBlocks.values.toList)
  }

  def processBlock(uuid: UUID): Unit = {
    val block = flow.block(uuid)
    val (locals, stack, ssaBlock) = prepareBlock(block)
    parseBlock(block, locals, stack, ssaBlock)
    finishBlock(block, locals, stack, ssaBlock)
    ssaBlocks(block.uuid) = ssaBlock
  }

  private def prepareBlock(block: SimpleBlock): (ListBuffer[Int], AStack[Int], SsaBlock) = {
    // get entry stuff
    val (locals, localPhis) = getLocalsAtStart(block)
    val (stack, stackPhi) = getStackAtStart(block)
    val ssaBlock = new SsaBlock(block.uuid, rawFlow = flow)

    // add entry φ
    localPhis.foreach(ssaBlock.inst += _)
    stackPhi.foreach(ssaBlock.inst += _)
    (locals, AStack(stack), ssaBlock)
  }

  private def finishBlock(block: SimpleBlock, locals: ListBuffer[Int], stack: AStack[Int], ssaBlock: SsaBlock): Unit = {
    // build end of node
    if (desiredLocals.contains(block.uuid)) {
      val desired = desiredLocals(block.uuid)
      for (i <- locals.indices) {
        ssaBlock.inst += ASSIGN(desired(i), locals(i))
      }
      exitMap(block.uuid) = ExitState(desired, stack.toList)
    } else {
      exitMap(block.uuid) = ExitState(locals.toList, stack.toList)
    }
    // put in successors
    for (uuid <- block.successors)
      ssaBlock.succ += uuid
  }

  private def parseBlock(block: SimpleBlock, locals: ListBuffer[Int], stack: AStack[Int], ssaBlock: SsaBlock): Unit = {
    if (block.stackframe == -1 && block.takesEntryParam) {
      ssaBlock.inst += OPERATION(RETURN, None, List(stack.pop()))
    } else {
      for (inst <- block.instructions) {
        inst match {
          case LOCAL_GET(id) =>
            val newSymbol = indexer.next
            ssaBlock.inst += ASSIGN(newSymbol, locals(id))
            stack.push(newSymbol)
          case LOCAL_TEE(id) =>
            val newSymbol = indexer.next
            ssaBlock.inst += ASSIGN(newSymbol, stack.head)
            locals(id) = newSymbol
          case LOCAL_SET(id) =>
            val newSymbol = indexer.next
            ssaBlock.inst += ASSIGN(newSymbol, stack.pop())
            locals(id) = newSymbol
          case DROP =>
            stack.pop()
          case NOP => // do nothing
          case x =>
            val (in, out) = determineSignature(x)
            val inputs = stack.pop(in)
            out match {
              case 0 => ssaBlock.inst += OPERATION(x, None, inputs)
              case 1 =>
                val newSymbol = indexer.next
                ssaBlock.inst += OPERATION(x, Some(newSymbol), inputs)
                stack.push(newSymbol)
            }
        }
      }
    }
  }

  private def determineSignature(inst: Instruction): (Int, Int) = inst match {
    case GLOBAL_GET(_) => 0 -> 1
    case GLOBAL_SET(_) => 1 -> 0
    case MEMORY_SIZE => 0 -> 1
    case MEMORY_GROW => 1 -> 1
    case MEMARG_INSTRUCTION(x, _, _) if x >= 0x28 && x <= 0x35 => 1 -> 1 // store
    case MEMARG_INSTRUCTION(x, _, _) if x >= 0x36 && x <= 0x3E => 2 -> 0 // load
    case CONST_INSTRUCTION(_, _) => 0 -> 1
    case IFELSE(_, _, _) => 1 -> 0
    case BRANCH(_) => 0 -> 0
    case BRANCH_COND(_) => 1 -> 0
    case BRANCH_TABLE(_, _) => 1 -> 0
    case RETURN => 0 -> 0
    case UNREACHABLE => 0 -> 0
    case SELECT => 3 -> 1
    case ARITHMETIC_INSTRUCTION(opcode) if NumericInstruction.binops contains opcode => 2 -> 1
    case ARITHMETIC_INSTRUCTION(opcode) if NumericInstruction.unops contains opcode => 1 -> 1
    case ARITHMETIC_INSTRUCTION(opcode) if NumericInstruction.testops contains opcode => 1 -> 1
    case ARITHMETIC_INSTRUCTION(opcode) if NumericInstruction.relops contains opcode => 2 -> 1
    case ARITHMETIC_INSTRUCTION(opcode) if NumericInstruction.convops contains opcode => 1 -> 1
    case CALL(fidx) => functionTypeSignature(fidx)
    case CALL_INDIRECT(typeId) => typeSignature(typeId)
  }

  def typeSignature(typeId: Int): (Int, Int) = {
    val typ = flow.module.types(typeId)
    if (typ.returnType.isEmpty) {
      typ.paramTypes.size -> 0
    } else {
      typ.paramTypes.size -> 1
    }
  }

  def functionTypeSignature(fId: Int): (Int, Int) = typeSignature(flow.module.funcs(fId).typeIdx)

}

object SsaParser {

  case class ExitState(locals: List[Int], stack: List[Int])

}