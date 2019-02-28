package ch.awae.wasm

package object ast {
  type DataStream = ch.awae.wasm.io.DataStream

  implicit def singleByte2DataStream(b: Byte): DataStream =
    new ch.awae.wasm.io.ListBackedDataStream(b :: Nil)

  implicit def byteList2DataStream(bs: List[Byte]): DataStream =
    new ch.awae.wasm.io.ListBackedDataStream(bs)

  implicit class AfterValueDo[T](val value: T) extends AnyVal {

    def after(f: => Any): T = {
      f
      value
    }

    def afterVerify(f: => Boolean): T = if (f) value else throw new AssertionError

    def afterGet[S](f: => S): S = f
  }

}