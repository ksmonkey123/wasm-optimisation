package ch.awae.wasm

package object io {
  object implicits {
    implicit class String2FileWrapper(val filename: String) extends AnyVal {
      def file = new java.io.File(filename)
    }
  }
}