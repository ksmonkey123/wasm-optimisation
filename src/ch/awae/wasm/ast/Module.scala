package ch.awae.wasm.ast

import scala.annotation.tailrec

case class Module(sections: List[Section])

case object Module {

  val signature: List[Byte] = List(0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00)

  def apply(stream: DataStream): Module =
    if (stream.take(8) != signature)
      throw new IllegalArgumentException
    else
      Module(readSections(stream))

  @tailrec
  def readSections(stream: DataStream, acc: List[Section] = Nil): List[Section] = stream.takeOptional match {
    case Some(x) => readSections(stream, Section(x, stream) :: acc)
    case None    => acc.reverse
  }
}