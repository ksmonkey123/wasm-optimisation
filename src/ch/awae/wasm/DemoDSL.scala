package ch.awae.wasm

import ch.awae.wasm.ast.Module
import ch.awae.wasm.ast.WasmFunction.DeclaredFunction
import ch.awae.wasm.ast.implicits._
import ch.awae.wasm.io.implicits._
import ch.awae.wasm.ssa.SsaParser
import ch.awae.wasm.util.Dot


class LoadedModule(val fs: List[DeclaredFunction], val module: Module) {
  def printCount: Unit = {
    val (total, memory) = fs.map(_.countIntructions).foldLeft((0, 0)) {
      case ((a, b), (x, y)) => (a + x, b + y)
    }
    println(s"# of Instructions: $total")
    println(s"# of Mem Instrucs: $memory")
  }

  def printFunctionCounts: Unit = {
    for {
      (f, index) <- fs.zipWithIndex
      (total, memory) = f.countIntructions
    } {
      println(s"$index\t$total\t$memory")
    }
  }

  def process(index: Int): Unit = {
    println(s"Showing function $index")
    val f = fs(index)
    val (total, memory) = f.countIntructions

    println(s"# of Instructions: $total")
    println(s"# of Mem Instrucs: $memory")

    val flow = cfg.Builder.build(f, module)
    Dot(flow.dot, "cfg-raw")

    flow.prune()
    Dot(flow.dot, "cfg-min")

    val ssa = new SsaParser(flow).parse()
    Dot(ssa.dot, "ssa-raw")

    ssa.prune()
    Dot(ssa.dot, "ssa-min")
  }
}

object load {
  def module(s: String) = {
    val module = s.file.ast.module
    val functions = module.funcs.filter(_.isInstanceOf[DeclaredFunction]).map(_.asInstanceOf[DeclaredFunction])

    println(s"loaded module $s")
    val x = new LoadedModule(functions, module)
    x.printCount
    x
  }
}
