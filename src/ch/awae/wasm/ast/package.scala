package ch.awae.wasm

package object ast {
  type DataStream = ch.awae.wasm.io.DataStream

  implicit private[ast] def singleByte2DataStream(b: Byte): DataStream =
    ch.awae.wasm.io.DataStream.ofList(b :: Nil)

  implicit private[ast] def byteList2DataStream(bs: List[Byte]): DataStream =
    ch.awae.wasm.io.DataStream.ofList(bs)

  implicit private[ast] class AfterValueDo[T](val value: T) extends AnyVal {
    def afterVerify(f: => Boolean): T = if (f) value else throw new AssertionError

    def afterGet[S](f: => S): S = f

    def >>=[S](f: T => S): S = f(value)
  }

  implicit private[ast] def int2byte(i: Int): Byte = (i & 0x000000ff).toByte

  object implicits {

    implicit class File2AstWrapper(val file: java.io.File) extends AnyVal {
      def ast: BinaryModule = Parser.parseFile(file)
    }

  }

}