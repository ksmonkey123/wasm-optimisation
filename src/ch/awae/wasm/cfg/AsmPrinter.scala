package ch.awae.wasm.cfg

import java.util

import ch.awae.wasm.ast.{Instruction, Module}
import ch.awae.wasm.ast.Instruction._

object AsmPrinter {

  def alignOffset(instruction: MEMARG_INSTRUCTION):String = s"(${instruction.a}, ${instruction.o})"

  def parseMemarg(instruction: MEMARG_INSTRUCTION): String = instruction.inst.toInt & 0x000000ff match {
    case 0x28 => "0x28 i32.load " + alignOffset(instruction)
    case 0x29 => "0x29 i64.load " + alignOffset(instruction)
    case 0x2a => "0x2a f32.load " + alignOffset(instruction)
    case 0x2b => "0x2b f64.load " + alignOffset(instruction)

    case 0x2c => "0x2c i32.load8_s " + alignOffset(instruction)
    case 0x2d => "0x2d i32.load8_u " + alignOffset(instruction)
    case 0x2e => "0x2e i32.load16_s " + alignOffset(instruction)
    case 0x2f => "0x2f i32.load16_u " + alignOffset(instruction)

    case 0x30 => "0x30 i64.load8_s " + alignOffset(instruction)
    case 0x31 => "0x31 i64.load8_u " + alignOffset(instruction)
    case 0x32 => "0x32 i64.load16_s " + alignOffset(instruction)
    case 0x33 => "0x33 i64.load16_u " + alignOffset(instruction)
    case 0x34 => "0x34 i64.load32_s " + alignOffset(instruction)
    case 0x35 => "0x35 i64.load32_u " + alignOffset(instruction)

    case 0x36 => "0x36 i32.store " + alignOffset(instruction)
    case 0x37 => "0x37 i64.store " + alignOffset(instruction)
    case 0x38 => "0x38 f32.store " + alignOffset(instruction)
    case 0x39 => "0x39 f64.store " + alignOffset(instruction)

    case 0x3a => "0x3a i32.store8 " + alignOffset(instruction)
    case 0x3b => "0x3b i32.store16 " + alignOffset(instruction)
    case 0x3c => "0x3c i64.store8 " + alignOffset(instruction)
    case 0x3d => "0x3d i64.store16 " + alignOffset(instruction)
    case 0x3e => "0x3e i64.store32 " + alignOffset(instruction)
    case 0x3f => "0x3f memory.size"
    case 0x40 => "0x40 memory.grow"
  }

  def parseConst(instruction: CONST_INSTRUCTION): String = instruction.inst.toInt & 0x000000ff match {
    case 0x41 => s"0x41 i32.const ${util.Arrays toString instruction.value.bytes.toArray}"
    case 0x42 => s"0x42 i64.const ${util.Arrays toString instruction.value.bytes.toArray}"
    case 0x43 => s"0x43 f32.const ${util.Arrays toString instruction.value.bytes.toArray}"
    case 0x44 => s"0x44 f64.const ${util.Arrays toString instruction.value.bytes.toArray}"
  }

  def parseArithmetic(instruction: ARITHMETIC_INSTRUCTION): String = instruction.inst.toInt & 0x000000ff match {
    case 0x45 => "0x45 i32.eqz"
    case 0x46 => "0x46 i32.eq"
    case 0x47 => "0x47 i32.ne"
    case 0x48 => "0x48 i32.lt_s"
    case 0x49 => "0x49 i32.lt_u"
    case 0x4a => "0x4a i32.gt_s"
    case 0x4b => "0x4b i32.gt_u"
    case 0x4c => "0x4c i32.le_s"
    case 0x4d => "0x4d i32.le_u"
    case 0x4e => "0x4e i32.ge_s"
    case 0x4f => "0x4f i32.ge_u"

    case 0x50 => "0x50 i64.eqz"
    case 0x51 => "0x51 i64.eq"
    case 0x52 => "0x52 i64.ne"
    case 0x53 => "0x53 i64.lt_s"
    case 0x54 => "0x54 i64.lt_u"
    case 0x55 => "0x55 i64.gt_s"
    case 0x56 => "0x56 i64.gt_u"
    case 0x57 => "0x57 i64.le_s"
    case 0x58 => "0x58 i64.le_u"
    case 0x59 => "0x59 i64.ge_s"
    case 0x5a => "0x5a i64.ge_u"

    case 0x5b => "0x5b f32.eq"
    case 0x5c => "0x5c f32.ne"
    case 0x5d => "0x5d f32.lt"
    case 0x5e => "0x5e f32.gt"
    case 0x5f => "0x5f f32.le"
    case 0x60 => "0x60 f32.ge"

    case 0x61 => "0x61 f64.eq"
    case 0x62 => "0x62 f64.ne"
    case 0x63 => "0x63 f64.lt"
    case 0x64 => "0x64 f64.gt"
    case 0x65 => "0x65 f64.le"
    case 0x66 => "0x66 f64.ge"

    case 0x67 => "0x67 i32.clz"
    case 0x68 => "0x68 i32.ctz"
    case 0x69 => "0x69 i32.popcnt"
    case 0x6a => "0x6a i32.add"
    case 0x6b => "0x6b i32.sub"
    case 0x6c => "0x6c i32.mul"
    case 0x6d => "0x6d i32.div_s"
    case 0x6e => "0x6e i32.div_u"
    case 0x6f => "0x6f i32.rem_s"
    case 0x70 => "0x70 i32.rem_u"
    case 0x71 => "0x71 i32.and"
    case 0x72 => "0x72 i32.or"
    case 0x73 => "0x73 i32.xor"
    case 0x74 => "0x74 i32.shl"
    case 0x75 => "0x75 i32.shr_s"
    case 0x76 => "0x76 i32.shr_u"
    case 0x77 => "0x77 i32.rotl"
    case 0x78 => "0x78 i32.rotr"

    case 0x79 => "0x79 i64.clz"
    case 0x7a => "0x7a i64.ctz"
    case 0x7b => "0x7b i64.popcnt"
    case 0x7c => "0x7c i64.add"
    case 0x7d => "0x7d i64.sub"
    case 0x7e => "0x7e i64.mul"
    case 0x7f => "0x7f i64.div_s"
    case 0x80 => "0x80 i64.div_u"
    case 0x81 => "0x81 i64.rem_s"
    case 0x82 => "0x82 i64.rem_u"
    case 0x83 => "0x83 i64.and"
    case 0x84 => "0x84 i64.or"
    case 0x85 => "0x85 i64.xor"
    case 0x86 => "0x86 i64.shl"
    case 0x87 => "0x87 i64.shr_s"
    case 0x88 => "0x88 i64.shr_u"
    case 0x89 => "0x89 i64.rotl"
    case 0x8a => "0x8a i64.rotr"

    case 0x8b => "0x8b f32.abs"
    case 0x8c => "0x8c f32.neg"
    case 0x8d => "0x8d f32.ceil"
    case 0x8e => "0x8e f32.floor"
    case 0x8f => "0x8f f32.trunc"
    case 0x90 => "0x90 f32.nearest"
    case 0x91 => "0x91 f32.sqrt"
    case 0x92 => "0x92 f32.add"
    case 0x93 => "0x93 f32.sub"
    case 0x94 => "0x94 f32.mul"
    case 0x95 => "0x95 f32.div"
    case 0x96 => "0x96 f32.min"
    case 0x97 => "0x97 f32.max"
    case 0x98 => "0x98 f32.copysign"

    case 0x99 => "0x99 f64.abs"
    case 0x9a => "0x9a f64.neg"
    case 0x9b => "0x9b f64.ceil"
    case 0x9c => "0x9c f64.floor"
    case 0x9d => "0x9d f64.trunc"
    case 0x9e => "0x9e f64.nearest"
    case 0x9f => "0x9f f64.sqrt"
    case 0xa0 => "0xa0 f64.add"
    case 0xa1 => "0xa1 f64.sub"
    case 0xa2 => "0xa2 f64.mul"
    case 0xa3 => "0xa3 f64.div"
    case 0xa4 => "0xa4 f64.min"
    case 0xa5 => "0xa5 f64.max"
    case 0xa6 => "0xa6 f64.copysign"

    case 0xa7 => "0xa7 i32.wrap_i64"
    case 0xa8 => "0xa8 i32.trunc_f32_s"
    case 0xa9 => "0xa9 i32.trunc_f32_u"
    case 0xaa => "0xaa i32.trunc_f64_s"
    case 0xab => "0xab i32.trunc_f64_u"
    case 0xac => "0xac i364.extend_i32_s"
    case 0xad => "0xad i364.extend_i32_u"
    case 0xae => "0xae i64.trunc_f32_s"
    case 0xaf => "0xaf i64.trunc_f32_u"
    case 0xb0 => "0xb0 i64.trunc_f64_s"
    case 0xb1 => "0xb1 i64.trunc_f64_u"
    case 0xb2 => "0xb2 f32.convert_i32_s"
    case 0xb3 => "0xb3 f32.convert_i32_u"
    case 0xb4 => "0xb4 f32.convert_i64_s"
    case 0xb5 => "0bx5 f32.convert_i64_u"
    case 0xb6 => "0xb6 f32.demote_f64"
    case 0xb7 => "0xb7 f64.convert_i32_s"
    case 0xb8 => "0xb8 f64.convert_i32_u"
    case 0xb9 => "0xb9 f64.convert_i64_s"
    case 0xba => "0bxa f64.convert_i64_u"
    case 0xbb => "0xbb f64.promote_f32"
    case 0xbc => "0xbc i32.reinterpret_f32"
    case 0xbd => "0xbd i64.reinterpret_f64"
    case 0xbe => "0xbe f32.reinterpret_i32"
    case 0xbf => "0xbf f64.reinterpret_i64"
  }

  def apply(instruction: Instruction, module:Module) : String = instruction match {

    case UNREACHABLE => "0x00 unreachable"
    case NOP => "0x01 nop"
    case IFELSE(_, _, _) => "0x04 if else"
    case BRANCH(label) => s"0x0c br $label"
    case BRANCH_COND(label) => s"0x0d br_if $label"
    case BRANCH_TABLE(table, default) => s"0x0e br_table ${table.foldRight(default.toString)(_+","+_)}"
    case RETURN => "0x0f return"
    case CALL(funcId) =>
      val signature = module.types(module.funcs(funcId).typeIdx)
      s"0x10 call $funcId : " + signature.paramTypes.size+ " => " + signature.returnType.getOrElse("void")
    case CALL_INDIRECT(typeId) =>
      val signature = module.types(typeId)
      s"0x11 call_indirect $typeId : " + signature.paramTypes.size+ " => " + signature.returnType.getOrElse("void")
    case DROP => "0x1a drop"
    case SELECT => "0x1b select"
    case LOCAL_GET(id) => s"0x20 local.get $id"
    case LOCAL_SET(id) => s"0x21 local.set $id"
    case LOCAL_TEE(id) => s"0x22 local.tee $id"
    case GLOBAL_GET(id) => s"0x23 global.get $id"
    case GLOBAL_SET(id) => s"0x24 global.set $id"
    case x : MEMARG_INSTRUCTION => parseMemarg(x)
    case x : CONST_INSTRUCTION => parseConst(x)
    case x : ARITHMETIC_INSTRUCTION => parseArithmetic(x)
    case x => x.getClass.getSimpleName
  }

}
