package ch.awae.wasm.ast

import ch.awae.wasm.ast.Code.Locals
import ch.awae.wasm.ast.NumericValue.I32
import ch.awae.wasm.ast.Section._
import ch.awae.wasm.ast.Types._

object Compiler {

  def compile(module: BinaryModule): List[Byte] = {
    BinaryModule.signature ::: module.sections.flatMap(compileSection)
  }

  def prependLength: List[Byte] => List[Byte] = c => I32(c.length).bytes ::: c

  def compileLimits: Types.Limit => List[Byte] = {
    case MinLimit(min) => 0x00 :: I32(min).bytes
    case MinMaxLimit(min, max) => 0x01 :: I32(min).bytes ::: I32(max).bytes
  }

  def compileImport: ImportEntry => List[Byte] = {
    case ImportEntry(mod, name, desc) => Name.compile(mod) ::: Name.compile(name) ::: (desc match {
      case FuncDesc(id) => 0x00.toByte :: I32(id).bytes
      case TableDesc(tt) => 0x01.toByte :: compileTableType(tt)
      case MemDesc(MemoryType(limits)) => 0x02.toByte :: compileLimits(limits)
      case GlobalDesc(GlobalType(valType, true)) => 0x03.toByte :: compileType(valType) ::: List(0x00.toByte)
      case GlobalDesc(GlobalType(valType, false)) => 0x03.toByte :: compileType(valType) ::: List(0x01.toByte)
    })
  }

  def compileIndex: Int => List[Byte] = I32(_).bytes

  def compileTableType: TableType => List[Byte] = {
    case TableType(limits) => 0x70.toByte :: compileLimits(limits)
  }

  def compileExpression(expr: Expression): List[Byte] = {
    expr.instructions.flatMap(compileInstruction) ::: (0x0b.toByte :: Nil)
  }

  def compileGlobalEntry: GlobalEntry => List[Byte] = {
    case GlobalEntry(GlobalType(valType, true), expr) => compileType(valType) ::: 0x00.toByte :: compileExpression(expr)
    case GlobalEntry(GlobalType(valType, false), expr) => compileType(valType) ::: 0x01.toByte :: compileExpression(expr)
  }

  def compileExport: ExportEntry => List[Byte] = {
    case ExportEntry(name, ExportType.Function(id)) => Name.compile(name) ::: 0.toByte :: I32(id).bytes
    case ExportEntry(name, ExportType.Table(id)) => Name.compile(name) ::: 1.toByte :: I32(id).bytes
    case ExportEntry(name, ExportType.Memory(id)) => Name.compile(name) ::: 2.toByte :: I32(id).bytes
    case ExportEntry(name, ExportType.Global(id)) => Name.compile(name) ::: 3.toByte :: I32(id).bytes
  }

  def compileElement: ElementEntry => List[Byte] = {
    case ElementEntry(tableId, expr, functions) => compileIndex(tableId) ::: compileExpression(expr) ::: Vec.compile(functions, compileIndex)
  }

  def compileLocals: Locals => List[Byte] = {
    case Locals(count, valType) => I32(count).bytes ::: compileType(valType)
  }

  def compileCode: Code => List[Byte] = {
    case Code(locals, body) => (Vec.compile(locals, compileLocals) ::: compileExpression(body)) >>= prependLength
  }

  def compileDataEntry: DataEntry => List[Byte] = {
    case DataEntry(memId, expr, data) => compileIndex(memId) ::: compileExpression(expr) ::: Vec.compile(data)
  }

  def compileSection: Section => List[Byte] = {
    case CustomSection(content) => 0 :: (content.toList >>= prependLength)
    case TypeSection(types) => 1 :: (Vec.compile(types, compileFunctionType) >>= prependLength)
    case ImportSection(imports) => 2 :: (Vec.compile(imports, compileImport) >>= prependLength)
    case FunctionSection(typeIndices) => 3 :: (Vec.compile(typeIndices, compileIndex) >>= prependLength)
    case TableSection(tableTypes) => 4 :: (Vec.compile(tableTypes, compileTableType) >>= prependLength)
    case MemorySection(memoryTypes) => 5 :: (Vec.compile(memoryTypes map (_.limits), compileLimits) >>= prependLength)
    case GlobalSection(globalEntries) => 6 :: (Vec.compile(globalEntries, compileGlobalEntry) >>= prependLength)
    case ExportSection(exports) => 7 :: (Vec.compile(exports, compileExport) >>= prependLength)
    case StartSection(idx) => 8 :: (I32(idx).bytes >>= prependLength)
    case ElementSection(elements) => 9 :: (Vec.compile(elements, compileElement) >>= prependLength)
    case CodeSection(functions) => 10 :: (Vec.compile(functions, compileCode) >>= prependLength)
    case DataSection(dataEntries) => 11 :: (Vec.compile(dataEntries, compileDataEntry) >>= prependLength)
  }

  def compileType: ResultType => List[Byte] = {
    case ResultType.VOID => List(0x40)
    case ValueType.i32 => List(0x7f)
    case ValueType.i64 => List(0x7e)
    case ValueType.f32 => List(0x7d)
    case ValueType.f64 => List(0x7c)
  }

  def compileFunctionType(typ: FunctionType): List[Byte] =
    0x60 :: Vec.compile(typ.paramTypes, compileType) ::: Vec.compile(typ.returnType.toList, compileType)

  def compileInstruction: Instruction => List[Byte] = {
    case Instruction.UNREACHABLE => 0 :: Nil
    case Instruction.NOP => 1 :: Nil
    case Instruction.BLOCK(ResultType.VOID, instructions) => 2 :: instructions.flatMap(compileInstruction) ::: List(0x0b.toByte)
    case Instruction.PLAIN_NUMERIC_INSTRUCTION(inst) => inst :: Nil
    case _ => Nil
  }

}
