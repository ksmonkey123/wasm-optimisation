package ch.awae.wasm.ast

import java.io.{BufferedInputStream, File, FileInputStream}

import ch.awae.wasm.io.{DataStream => DS}

import scala.util.control.NonFatal

object Parser {

  def parseFile(file: File): BinaryModule = {
    val inputStream = new BufferedInputStream(new FileInputStream(file))
    try {
      val stream = DS.ofStream(inputStream)
      BinaryModule(stream)
    } finally {
      try {
        inputStream.close()
      } catch {
        case NonFatal(_) =>
      }
    }

  }
}