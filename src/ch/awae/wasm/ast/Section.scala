package ch.awae.wasm.ast

sealed trait ImportDescription

case class TypeDesc(typeId: I32) extends ImportDescription
case class TableDesc(tableType: TableType) extends ImportDescription
case class MemDesc(memType: MemoryType) extends ImportDescription
case class GlobalDesc(globalType: GlobalType) extends ImportDescription

object ImportDescription {
  def apply(stream: DataStream) = stream.take match {
    case 0x00 => TypeDesc(I32(stream))
    case 0x01 => TableDesc(TableType(stream))
    case 0x02 => MemDesc(MemoryType(stream))
    case 0x03 => GlobalDesc(GlobalType(stream))
  }
}

case class ImportEntry(mod: Name, name: Name, desc: ImportDescription)
object ImportEntry {
  def apply(stream: DataStream): ImportEntry = ImportEntry(Name(stream), Name(stream), ImportDescription(stream))
}

case class GlobalEntry(gt: GlobalType, expr: Expression)
object GlobalEntry {
  def apply(stream: DataStream): GlobalEntry = GlobalEntry(GlobalType(stream), Expression(stream))
}

sealed trait ExportType
object ExportType {
  case object FUNCTION extends ExportType
  case object TABLE extends ExportType
  case object MEMORY extends ExportType
  case object GLOBAL extends ExportType
}

case class ExportEntry(name: Name, expType: ExportType, idx: I32)
object ExportEntry {
  def apply(stream: DataStream): ExportEntry = {
    val name = Name(stream)
    val typ = stream.take match {
      case 0x00 => ExportType.FUNCTION
      case 0x01 => ExportType.TABLE
      case 0x02 => ExportType.MEMORY
      case 0x03 => ExportType.GLOBAL
    }
    val idx = I32(stream)
    ExportEntry(name, typ, idx)
  }
}

case class ElementEntry(tableId: I32, expr: Expression, functions: Vec[I32])
object ElementEntry {
  def apply(stream: DataStream): ElementEntry = ElementEntry(I32(stream), Expression(stream), Vec(stream, I32(_)))
}

case class DataEntry(memId: I32, expr: Expression, data: Vec[Byte])
object DataEntry {
  def apply(stream: DataStream): DataEntry = DataEntry(I32(stream), Expression(stream), Vec(stream, _.take))
}

trait Section

case class CustomSection(content: Array[Byte]) extends Section
case class TypeSection(types: Vec[FunctionType]) extends Section
case class ImportSection(imports: Vec[ImportEntry]) extends Section
case class FunctionSection(typeIndices: Vec[I32]) extends Section
case class TableSection(tableTypes: Vec[TableType]) extends Section
case class MemorySection(memTypes: Vec[MemoryType]) extends Section
case class GlobalSection(entries: Vec[GlobalEntry]) extends Section
case class ExportSection(exports: Vec[ExportEntry]) extends Section
case class StartSection(idx: I32) extends Section
case class ElementSection(elements: Vec[ElementEntry]) extends Section
case class CodeSection(functions: Vec[Code]) extends Section
case class DataSection(entries: Vec[DataEntry]) extends Section

object Section {
  def apply(sectionType: Byte, stream: DataStream): Section = {
    val length = I32(stream).unsigned
    sectionType match {
      case 0x00 => CustomSection(stream.take(length).toArray)
      case 0x01 => TypeSection(Vec(stream, FunctionType(_)))
      case 0x02 => ImportSection(Vec(stream, ImportEntry(_)))
      case 0x03 => FunctionSection(Vec(stream, I32(_)))
      case 0x04 => TableSection(Vec(stream, TableType(_)))
      case 0x05 => MemorySection(Vec(stream, MemoryType(_)))
      case 0x06 => GlobalSection(Vec(stream, GlobalEntry(_)))
      case 0x07 => ExportSection(Vec(stream, ExportEntry(_)))
      case 0x08 => StartSection(I32(stream))
      case 0x09 => ElementSection(Vec(stream, ElementEntry(_)))
      case 0x0a => CodeSection(Vec(stream, Code(_)))
      case 0x0b => DataSection(Vec(stream, DataEntry(_)))
    }
  }
}

