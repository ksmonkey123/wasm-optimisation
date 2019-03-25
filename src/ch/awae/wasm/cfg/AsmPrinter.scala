package ch.awae.wasm.cfg

import java.util

import ch.awae.wasm.ast.Instruction
import ch.awae.wasm.ast.Instruction._

object AsmPrinter {

  def parseMemarg(instruction: MEMARG_INSTRUCTION): String = instruction.inst.toInt & 0x000000ff match {
    case 0x28 => "0x28 i32.load"
    case 0x29 => "0x29 i64.load"
    case 0x2a => "0x2a f32.load"
    case 0x2b => "0x2b f64.load"

    case 0x2c => "0x2c i32.load8_s"
    case 0x2d => "0x2d i32.load8_u"
    case 0x2e => "0x2e i32.load16_s"
    case 0x2f => "0x2f i32.load16_u"

    case 0x30 => "0x30 i64.load8_s"
    case 0x31 => "0x31 i64.load8_u"
    case 0x32 => "0x32 i64.load16_s"
    case 0x33 => "0x33 i64.load16_u"
    case 0x34 => "0x34 i64.load32_s"
    case 0x35 => "0x35 i64.load32_u"

    case 0x36 => "0x36 i32.store"
    case 0x37 => "0x37 i64.store"
    case 0x38 => "0x38 f32.store"
    case 0x39 => "0x39 f64.store"

    case 0x3a => "0x3a i32.store8"
    case 0x3b => "0x3b i32.store16"
    case 0x3c => "0x3c i64.store8"
    case 0x3d => "0x3d i64.store16"
    case 0x3e => "0x3e i64.store32"
    case 0x3f => "0x3f memory.size"
    case 0x40 => "0x40 memory.grow"
  }

  def parseConst(instruction: CONST_INSTRUCTION): String = instruction.inst.toInt & 0x000000ff match {
    case 0x41 => s"0x41 i32.const ${util.Arrays toString instruction.value.bytes.toArray}"
    case 0x42 => s"0x42 i64.const ${util.Arrays toString instruction.value.bytes.toArray}"
    case 0x43 => s"0x43 f32.const ${util.Arrays toString instruction.value.bytes.toArray}"
    case 0x44 => s"0x44 f64.const ${util.Arrays toString instruction.value.bytes.toArray}"
  }

  def apply(instruction: Instruction) : String = instruction match {

    case UNREACHABLE => "0x00 unreachable"
    case NOP => "0x01 nop"
    case IFELSE(_, _, _) => "0x04 if else"
    case BRANCH(label) => s"0x0c br $label"
    case BRANCH_COND(label) => s"0x0d br_if $label"
    case BRANCH_TABLE(table, default) => s"0x0e br_table ${table.foldRight(default.toString)(_+","+_)}"
    case RETURN => "0x0f return"
    case CALL(funcId) => s"0x10 call $funcId"
    case CALL_INDIRECT(typeId) => s"0x11 call_indirect $typeId"
    case DROP => "0x1a drop"
    case SELECT => "0x1b select"
    case LOCAL_GET(id) => s"0x20 local.get $id"
    case LOCAL_SET(id) => s"0x21 local.set $id"
    case LOCAL_TEE(id) => s"0x22 local.tee $id"
    case GLOBAL_GET(id) => s"0x23 global.get $id"
    case GLOBAL_SET(id) => s"0x24 global.set $id"
    case x : MEMARG_INSTRUCTION => parseMemarg(x)
    case x : CONST_INSTRUCTION => parseConst(x)
    case x => x.getClass.getSimpleName
  }

}
