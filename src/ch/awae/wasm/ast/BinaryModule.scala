package ch.awae.wasm.ast

import scala.annotation.tailrec
import scala.reflect.ClassTag

case class BinaryModule(sections: List[Section]) {
  def module = Module(this)

  private[ast] def selectAll[T <: Section](implicit ev: ClassTag[T]) = (sections.filter(ev.unapply(_).isDefined).map(ev.unapply(_).get).headOption, BinaryModule(sections.filterNot(ev.unapply(_).isDefined)))
}

case object BinaryModule {

  val signature: List[Byte] = List(0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00) map (_.toByte)

  def apply(stream: DataStream): BinaryModule =
    if (stream.take(8) != signature)
      throw new IllegalArgumentException
    else
      BinaryModule(readSections(stream))

  @tailrec
  private def readSections(stream: DataStream, acc: List[Section] = Nil): List[Section] = stream.takeOptional match {
    case Some(x) => readSections(stream, Section(x, stream) :: acc)
    case None => acc.reverse
  }
}