package ch.awae.wasm.cfg

import ch.awae.wasm.ast.Instruction
import ch.awae.wasm.ast.Instruction._
import ch.awae.wasm.ast.WasmFunction.DeclaredFunction

object Builder {


  def build(function: DeclaredFunction):ControlFlow = {
    val flow = new ControlFlow
    // add global return block
    val returnBlock = new SimpleBlock
    flow += returnBlock
    // main block
    val mainBlock = new SimpleBlock
    flow += mainBlock
    // parse method body
    parseInstructions(function.body, mainBlock, returnBlock, returnBlock, returnBlock::Nil)(flow)

    flow
  }


  def parseInstructions(instructions: List[Instruction],
                        currentBlock: SimpleBlock,
                        successorBlock: SimpleBlock,
                        returnBlock: SimpleBlock,
                        branchTargets: List[SimpleBlock])
                       (implicit cfg : ControlFlow):Unit = {
    instructions match {
      case Nil =>
        currentBlock.successors = List(successorBlock.uuid)
      case (x:BlockInstruction) :: xs =>
        val succ = SimpleBlock()
        val inner = parseBlock(x, succ, returnBlock, branchTargets)
        currentBlock.successors = List(inner.uuid)
        parseInstructions(xs, succ, successorBlock, returnBlock, branchTargets)
      // HANDLE BRANCHING
      case RETURN :: xs =>
        currentBlock += RETURN
        currentBlock.successors = List(returnBlock.uuid)
        parseInstructions(xs, SimpleBlock(), successorBlock, returnBlock, branchTargets)
      case BREAK(index) :: xs =>
        currentBlock += BREAK(index)
        currentBlock.successors = List(branchTargets(index).uuid)
        parseInstructions(xs, SimpleBlock(), successorBlock, returnBlock, branchTargets)
      case BREAK_COND(index) :: xs =>
        currentBlock += BREAK_COND(index)
        val succ = SimpleBlock()
        currentBlock.successors = List(branchTargets(index).uuid, succ.uuid)
        parseInstructions(xs, succ, successorBlock, returnBlock, branchTargets)
      case BREAK_TABLE(table, default) :: xs =>
        currentBlock += instructions.head
        currentBlock.successors = (table.map(branchTargets) ::: branchTargets(default) :: Nil).map(_.uuid)
        parseInstructions(xs, SimpleBlock(), successorBlock, returnBlock, branchTargets)
      case UNREACHABLE :: xs =>
        currentBlock += UNREACHABLE
        parseInstructions(xs, SimpleBlock(), successorBlock, returnBlock, branchTargets)
      // DEFAULT CASE
      case x :: xs =>
        currentBlock += x
        parseInstructions(xs, currentBlock, successorBlock, returnBlock, branchTargets)
    }

  }

  def parseBlock(block: BlockInstruction,
                 successor: SimpleBlock,
                 returnBlock: SimpleBlock,
                 branchTargets: List[SimpleBlock])
                (implicit flow: ControlFlow):SimpleBlock = {
    block match {
      case BLOCK(_, instructions) =>
        val sb = SimpleBlock()
        parseInstructions(instructions, sb, successor, returnBlock, successor :: branchTargets)
        sb
      case LOOP(_, instructions) =>
        val sb = SimpleBlock()
        parseInstructions(instructions, sb, successor, returnBlock, sb :: branchTargets)
        sb
      case IFELSE(_, ifBlock, elseBlock) =>
        val branchBlock = SimpleBlock()
        branchBlock += block
        val inner_if = SimpleBlock()
        parseInstructions(ifBlock, inner_if, successor, returnBlock, successor :: branchTargets)
        val inner_else = SimpleBlock()
        parseInstructions(elseBlock, inner_else, successor, returnBlock, successor :: branchTargets)
        branchBlock.successors = List(inner_if.uuid, inner_else.uuid)
        branchBlock
    }
  }


}
