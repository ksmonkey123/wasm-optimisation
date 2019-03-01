package ch.awae.wasm.ast

sealed trait ImportDescription

case class FuncDesc(typeId: Int) extends ImportDescription

case class TableDesc(tableType: TableType) extends ImportDescription

case class MemDesc(memType: MemoryType) extends ImportDescription

case class GlobalDesc(globalType: GlobalType) extends ImportDescription

object ImportDescription {
  private[ast] def apply(stream: DataStream) = stream.take match {
    case 0x00 => FuncDesc(I32(stream).unsigned)
    case 0x01 => TableDesc(TableType(stream))
    case 0x02 => MemDesc(MemoryType(stream))
    case 0x03 => GlobalDesc(GlobalType(stream))
  }
}

case class ImportEntry(mod: String, name: String, desc: ImportDescription)

object ImportEntry {
  private[ast] def apply(stream: DataStream): ImportEntry = ImportEntry(Name(stream), Name(stream), ImportDescription(stream))
}

case class GlobalEntry(gt: GlobalType, expr: Expression)

object GlobalEntry {
  private[ast] def apply(stream: DataStream): GlobalEntry = GlobalEntry(GlobalType(stream), Expression(stream))
}

sealed trait ExportType

object ExportType {

  case class Function(id: Int) extends ExportType

  case class Table(id: Int) extends ExportType

  case class Memory(id: Int) extends ExportType

  case class Global(id: Int) extends ExportType

}

case class ExportEntry(name: String, expType: ExportType)

object ExportEntry {
  private[ast] def apply(stream: DataStream): ExportEntry = {
    val name = Name(stream)
    val typ = stream.take
    val id = I32(stream).unsigned
    val exportType = typ match {
      case 0x00 => ExportType.Function(id)
      case 0x01 => ExportType.Table(id)
      case 0x02 => ExportType.Memory(id)
      case 0x03 => ExportType.Global(id)
    }
    ExportEntry(name, exportType)
  }
}

case class ElementEntry(tableId: Int, expr: Expression, functions: List[Int])

object ElementEntry {
  private[ast] def apply(stream: DataStream): ElementEntry = ElementEntry(I32(stream).unsigned, Expression(stream), Vec(stream, I32(_).unsigned))
}

case class DataEntry(memId: Int, expr: Expression, data: List[Byte])

object DataEntry {
  private[ast] def apply(stream: DataStream): DataEntry = DataEntry(I32(stream).unsigned, Expression(stream), Vec(stream, _.take))
}

trait Section

case class CustomSection(content: Array[Byte]) extends Section

case class TypeSection(types: List[FunctionType]) extends Section

case class ImportSection(imports: List[ImportEntry]) extends Section

case class FunctionSection(typeIndices: List[Int]) extends Section

case class TableSection(tableTypes: List[TableType]) extends Section

case class MemorySection(memTypes: List[MemoryType]) extends Section

case class GlobalSection(entries: List[GlobalEntry]) extends Section

case class ExportSection(exports: List[ExportEntry]) extends Section

case class StartSection(idx: Int) extends Section

case class ElementSection(elements: List[ElementEntry]) extends Section

case class CodeSection(functions: List[Code]) extends Section

case class DataSection(entries: List[DataEntry]) extends Section

object Section {
  private[ast] def apply(sectionType: Byte, stream: DataStream): Section = {
    val length = I32(stream).unsigned
    sectionType match {
      case 0x00 => CustomSection(stream.take(length).toArray)
      case 0x01 => TypeSection(Vec(stream, FunctionType(_)))
      case 0x02 => ImportSection(Vec(stream, ImportEntry(_)))
      case 0x03 => FunctionSection(Vec(stream, I32(_).unsigned))
      case 0x04 => TableSection(Vec(stream, TableType(_)))
      case 0x05 => MemorySection(Vec(stream, MemoryType(_)))
      case 0x06 => GlobalSection(Vec(stream, GlobalEntry(_)))
      case 0x07 => ExportSection(Vec(stream, ExportEntry(_)))
      case 0x08 => StartSection(I32(stream).unsigned)
      case 0x09 => ElementSection(Vec(stream, ElementEntry(_)))
      case 0x0a => CodeSection(Vec(stream, Code(_)))
      case 0x0b => DataSection(Vec(stream, DataEntry(_)))
    }
  }
}
