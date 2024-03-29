package ch.awae.wasm.cfg

import ch.awae.wasm.ast.Instruction._
import ch.awae.wasm.ast.WasmFunction.DeclaredFunction
import ch.awae.wasm.ast.{Instruction, Module}
import ch.awae.wasm.util.{Indexer, SequentialIndexer}

object Builder {


  def build(function: DeclaredFunction, module: Module):ControlFlow = {
    val flow = new ControlFlow(module, module.types(function.typeIdx), function.locals)
    val frameIndexer = new SequentialIndexer(1)
    // add global return block
    val returnBlock = new SimpleBlock(flow, stackframe = -1)
    returnBlock.entryType = module.types(function.typeIdx).returnType
    flow += returnBlock
    flow.end = returnBlock.uuid
    // main block
    val mainBlock = new SimpleBlock(flow, stackframe = 0)
    flow += mainBlock
    flow.start = mainBlock.uuid
    // parse method body
    parseInstructions(function.body, mainBlock, returnBlock, returnBlock, returnBlock::Nil)(flow, frameIndexer)

    flow
  }


  def parseInstructions(instructions: List[Instruction],
                        currentBlock: SimpleBlock,
                        successorBlock: SimpleBlock,
                        returnBlock: SimpleBlock,
                        branchTargets: List[SimpleBlock])
                       (implicit cfg : ControlFlow,
                        stackIndexer : Indexer[Int]):Unit = {
    instructions match {
      case Nil =>
        currentBlock.successors = List(successorBlock.uuid)
      case (x:BlockInstruction) :: xs =>
        val succ = SimpleBlock(currentBlock.stackframe, stackPredecessor = currentBlock.uuid)
        val inner = parseBlock(x, succ, returnBlock, branchTargets, currentBlock.stackframe)
        currentBlock.successors = List(inner.uuid)
        parseInstructions(xs, succ, successorBlock, returnBlock, branchTargets)
      // HANDLE BRANCHING
      case RETURN :: xs =>
        currentBlock += RETURN
        currentBlock.successors = List(returnBlock.uuid)
        parseInstructions(xs, SimpleBlock(currentBlock.stackframe, stackPredecessor = currentBlock.uuid), successorBlock, returnBlock, branchTargets)
      case BRANCH(index) :: xs =>
        currentBlock += BRANCH(index)
        currentBlock.successors = List(branchTargets(index).uuid)
        parseInstructions(xs, SimpleBlock(currentBlock.stackframe, stackPredecessor = currentBlock.uuid), successorBlock, returnBlock, branchTargets)
      case BRANCH_COND(index) :: xs =>
        currentBlock += BRANCH_COND(index)
        val succ = SimpleBlock(currentBlock.stackframe, stackPredecessor = currentBlock.uuid)
        currentBlock.successors = List(branchTargets(index).uuid, succ.uuid)
        parseInstructions(xs, succ, successorBlock, returnBlock, branchTargets)
      case BRANCH_TABLE(table, default) :: xs =>
        currentBlock += instructions.head
        currentBlock.successors = (table.map(branchTargets) ::: branchTargets(default) :: Nil).map(_.uuid)
        parseInstructions(xs, SimpleBlock(currentBlock.stackframe, stackPredecessor = currentBlock.uuid), successorBlock, returnBlock, branchTargets)
      case UNREACHABLE :: xs =>
        currentBlock += UNREACHABLE
        parseInstructions(xs, SimpleBlock(currentBlock.stackframe, stackPredecessor = currentBlock.uuid), successorBlock, returnBlock, branchTargets)
      // DEFAULT CASE
      case x :: xs =>
        currentBlock += x
        parseInstructions(xs, currentBlock, successorBlock, returnBlock, branchTargets)
    }

  }

  def parseBlock(block: BlockInstruction,
                 successor: SimpleBlock,
                 returnBlock: SimpleBlock,
                 branchTargets: List[SimpleBlock],
                 parentStackframe : Int)
                (implicit flow: ControlFlow,
                 stackIndexer: Indexer[Int]):SimpleBlock = {
    block match {
      case BLOCK(resultType, instructions) =>
        val sb = SimpleBlock(stackIndexer.next)
        successor.entryType = resultType.valueType
        parseInstructions(instructions, sb, successor, returnBlock, successor :: branchTargets)
        sb
      case LOOP(resultType, instructions) =>
        val sb = SimpleBlock(stackIndexer.next, loopHead = true)
        successor.entryType = resultType.valueType
        parseInstructions(instructions, sb, successor, returnBlock, sb :: branchTargets)
        sb
      case IFELSE(resultType, ifBlock, elseBlock) =>
        successor.entryType = resultType.valueType
        val branchBlock = SimpleBlock(parentStackframe)
        branchBlock += block
        val inner_if = SimpleBlock(stackIndexer.next)
        parseInstructions(ifBlock, inner_if, successor, returnBlock, successor :: branchTargets)
        val inner_else = SimpleBlock(stackIndexer.next)
        parseInstructions(elseBlock, inner_else, successor, returnBlock, successor :: branchTargets)
        branchBlock.successors = List(inner_if.uuid, inner_else.uuid)
        branchBlock
    }
  }


}
