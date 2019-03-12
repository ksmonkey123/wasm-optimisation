package ch.awae.wasm

import java.nio.file.Files

package object io {

  object implicits {

    implicit class String2FileWrapper(val filename: String) extends AnyVal {
      def file = new java.io.File(filename)
    }

    implicit class ByteList2FileWriter(val stream: List[Byte]) extends AnyVal {
      def write(filename: String): Unit = Files.write(filename.file.toPath, stream.toArray)
    }

  }

}