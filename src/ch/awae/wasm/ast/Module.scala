package ch.awae.wasm.ast

case class Module(types: List[FunctionType], funcs: List[WasmFunction], remainder: List[Section]) {
  def ast: BinaryModule = {
    val (dataSection, temp) = BinaryModule(this.remainder).selectAll[DataSection]
    val (iprtSection, remainder) = temp.selectAll[ImportSection]

    val (fnx, code) = funcs map {
      case DeclaredFunction(idx, locals, code) => (idx, Code(locals, Expression(code)))
      case _                                   => null
    } filter (_ != null) unzip

    var sections: List[Section] = Nil

    if (dataSection.isDefined) sections ::= dataSection.get
    if (!code.isEmpty) sections ::= CodeSection(code)
    if (!remainder.sections.isEmpty) sections :::= remainder.sections
    if (!fnx.isEmpty) sections ::= FunctionSection(fnx)
    if (iprtSection.isDefined) sections ::= iprtSection.get
    if (!types.isEmpty) sections ::= TypeSection(types)

    BinaryModule(sections)
  }
}

object Module {
  private[ast] def apply(raw0: BinaryModule): Module = {
    val (types, raw1) = raw0.selectAll[TypeSection]
    val (funcs, raw2) = raw1.selectAll[FunctionSection]
    val (imprs, raw3) = raw2.selectAll[ImportSection]
    val (codes, raw4) = raw3.selectAll[CodeSection]
    val remainder = (imprs map (_ :: Nil) getOrElse Nil) ::: raw4.sections

    val localFunctions =
      if (funcs.isDefined && codes.isDefined)
        funcs.get.typeIndices zip codes.get.functions map {
          case (idx, Code(locals, body)) => DeclaredFunction(idx, locals, body.instructions)
        }
      else
        Nil

    val importedFunctions =
      if (imprs.isDefined)
        imprs.get.imports filter (_.desc.isInstanceOf[FuncDesc]) map {
          case ImportEntry(mod, name, FuncDesc(idx)) => ImportedFunction(idx, mod, name)
          case _                                     => ???
        }
      else Nil

    Module(types map (_.types) getOrElse Nil, importedFunctions ::: localFunctions, remainder)
  }

}