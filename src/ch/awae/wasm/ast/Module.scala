package ch.awae.wasm.ast

import scala.annotation.tailrec

case class RawModule(sections: List[Section])

case object RawModule {

  val signature: List[Byte] = List(0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00)

  private[ast] def apply(stream: DataStream): RawModule =
    if (stream.take(8) != signature)
      throw new IllegalArgumentException
    else
      RawModule(readSections(stream))

  @tailrec
  private def readSections(stream: DataStream, acc: List[Section] = Nil): List[Section] = stream.takeOptional match {
    case Some(x) => readSections(stream, Section(x, stream) :: acc)
    case None    => acc.reverse
  }
}

case class Module(rawSections: List[Section], types: TypeSection, functions: List[WasmFunction])

object Module {
  private[ast] def apply(stream: DataStream): Module = {
    @tailrec
    def scan(
      sections:  List[Section],
      remainder: List[Section]   = Nil,
      types:     TypeSection     = null,
      funcs:     FunctionSection = null,
      codes:     CodeSection     = null,
      impos:     ImportSection   = null): Module = {
      if (sections.isEmpty)
        collect(remainder.reverse, types, funcs, codes, impos)
      else
        sections.head match {
          case x: TypeSection     => scan(sections.tail, remainder, x, funcs, codes, impos)
          case x: FunctionSection => scan(sections.tail, remainder, types, x, codes, impos)
          case x: CodeSection     => scan(sections.tail, remainder, types, funcs, x, impos)
          case x: ImportSection   => scan(sections.tail, remainder, types, funcs, codes, x)
          case x                  => scan(sections.tail, x :: remainder, types, funcs, codes, impos)
        }
    }

    def collect(remainder: List[Section], types: TypeSection, funcs: FunctionSection, codes: CodeSection, impos: ImportSection): Module = {

      val declaredFunctions =
        if (funcs != null)
          funcs.typeIndices zip codes.functions map {
            case (tIdx, Code(locals, body)) => DeclaredFunction(tIdx, locals, body)
          }
        else Nil
      val otherImports = Option(impos) map { _.imports filter { !_.desc.isInstanceOf[TypeDesc] } }
      val functionImports = Option(impos) map {
        _.imports filter { _.desc.isInstanceOf[TypeDesc] } map {
          case ImportEntry(mod, name, TypeDesc(typeId)) => ImportedFunction(typeId, mod, name)
          case _                                        => ???
        }
      }

      Module(if (otherImports.isDefined) ImportSection(otherImports.get) :: remainder else remainder, types,
        if (functionImports.isDefined) functionImports.get ::: declaredFunctions else declaredFunctions)
    }

    scan(RawModule(stream).sections)

  }

}