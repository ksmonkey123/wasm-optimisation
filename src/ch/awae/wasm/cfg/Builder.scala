package ch.awae.wasm.cfg

import ch.awae.wasm.ast.Instruction
import ch.awae.wasm.ast.Instruction._
import ch.awae.wasm.ast.WasmFunction.DeclaredFunction
import ch.awae.wasm.util.{Indexer, SequentialIndexer}

object Builder {


  def build(function: DeclaredFunction):ControlFlow = {
    val flow = new ControlFlow
    val frameIndexer = new SequentialIndexer(1)
    // add global return block
    val returnBlock = new SimpleBlock(flow, stackframe = -1)
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
        val succ = SimpleBlock(currentBlock.stackframe)
        val inner = parseBlock(x, succ, returnBlock, branchTargets, currentBlock.stackframe)
        currentBlock.successors = List(inner.uuid)
        parseInstructions(xs, succ, successorBlock, returnBlock, branchTargets)
      // HANDLE BRANCHING
      case RETURN :: xs =>
        currentBlock += RETURN
        currentBlock.successors = List(returnBlock.uuid)
        parseInstructions(xs, SimpleBlock(currentBlock.stackframe), successorBlock, returnBlock, branchTargets)
      case BRANCH(index) :: xs =>
        currentBlock += BRANCH(index)
        currentBlock.successors = List(branchTargets(index).uuid)
        parseInstructions(xs, SimpleBlock(currentBlock.stackframe), successorBlock, returnBlock, branchTargets)
      case BRANCH_COND(index) :: xs =>
        currentBlock += BRANCH_COND(index)
        val succ = SimpleBlock(currentBlock.stackframe)
        currentBlock.successors = List(branchTargets(index).uuid, succ.uuid)
        parseInstructions(xs, succ, successorBlock, returnBlock, branchTargets)
      case BRANCH_TABLE(table, default) :: xs =>
        currentBlock += instructions.head
        currentBlock.successors = (table.map(branchTargets) ::: branchTargets(default) :: Nil).map(_.uuid)
        parseInstructions(xs, SimpleBlock(currentBlock.stackframe), successorBlock, returnBlock, branchTargets)
      case UNREACHABLE :: xs =>
        currentBlock += UNREACHABLE
        parseInstructions(xs, SimpleBlock(currentBlock.stackframe), successorBlock, returnBlock, branchTargets)
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
      case BLOCK(_, instructions) =>
        val sb = SimpleBlock(stackIndexer.next)
        parseInstructions(instructions, sb, successor, returnBlock, successor :: branchTargets)
        sb
      case LOOP(_, instructions) =>
        val sb = SimpleBlock(stackIndexer.next)
        parseInstructions(instructions, sb, successor, returnBlock, sb :: branchTargets)
        sb
      case IFELSE(_, ifBlock, elseBlock) =>
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
