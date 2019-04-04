package ch.awae.wasm

import ch.awae.wasm.ast.WasmFunction.DeclaredFunction
import ch.awae.wasm.ast.implicits._
import ch.awae.wasm.io.implicits._
import ch.awae.wasm.util.Dot
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.jgrapht.traverse.TopologicalOrderIterator

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.collection.mutable.ListBuffer

object Main extends App {

  val graph = new DefaultDirectedGraph[String, DefaultEdge](classOf[DefaultEdge]);

  graph.addVertex("a")
  graph.addVertex("b")
  graph.addVertex("c")
  graph.addVertex("d")
  graph.addVertex("e")
  graph.addVertex("f")

  graph.addEdge("a", "b")
  graph.addEdge("a", "c")
  graph.addEdge("b", "c")
  graph.addEdge("e", "b")
  graph.addEdge("c", "f")
  graph.addEdge("e", "f")
  graph.addEdge("b", "d")

  val iterator = new TopologicalOrderIterator(graph)
  val nodes = ListBuffer.empty
  val ordering = iterator.asScala.toList

  println(ordering)

  System exit 0

  val module = "change.wasm".file.ast.module
  val functions = module.funcs.filter(_.isInstanceOf[DeclaredFunction]).map(_.asInstanceOf[DeclaredFunction])

  //  7 - very long
  // 10 - complex
  // 16 - simple nested branching
  // 18 - deep nesting
  // 20 - huuuge

  val f = functions apply 10

  val flow = cfg.Builder.build(f, module).pruned()

  println(f)

  Dot(flow.dot)

}