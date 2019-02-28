package ch.awae.wasm.ast

import java.io.BufferedInputStream
import java.io.FileInputStream
import scala.util.control.NonFatal
import ch.awae.wasm.io.{ DataStream => DS }
import java.io.File

object Parser {

  def parseFile(file: File) = {
    val inputStream = new BufferedInputStream(new FileInputStream(file))
    try {
      val stream = DS.ofStream(inputStream)
      Module(stream)
    } finally {
      try {
        inputStream.close
      } catch {
        case NonFatal(_) =>
      }
    }
  }

}