package ch.awae.wasm.io

import java.io.InputStream

sealed trait DataStream {
  def take: Byte
  def take(n: Int): List[Byte] = {
    for {
      i <- 1 to n
    } yield {
      take
    }
  }.toList
  def takeOptional: Option[Byte] = try {
    Some(take)
  } catch {
    case _: NoSuchElementException => None
  }

  def ::(x: Byte): DataStream = new CompositeDataStream(x :: Nil, this)
  def :::(xs: List[Byte]): DataStream = new CompositeDataStream(xs, this)
}

object DataStream {
  def ofStream(s : InputStream):DataStream = new InputStreamDataStream(s)
}

class CompositeDataStream(var list: List[Byte], val parent: DataStream) extends DataStream {

  override def ::(x: Byte): DataStream = {
    list = x :: list
    this
  }

  override def :::(xs: List[Byte]): DataStream = {
    list = xs ::: list
    this
  }

  def take = {
    if (list.isEmpty)
      parent.take
    else {
      val next = list.head
      list = list.tail
      next
    }
  }

}

class ListBackedDataStream(@volatile private[this] var list: List[Byte]) extends DataStream {
  def take = {
    val next = list.head
    list = list.tail
    next
  }
}

class InputStreamDataStream(stream: InputStream) extends DataStream {
  def take = {
    try {
      val next = stream.read
      if (next == -1)
        throw new NoSuchElementException
      next.toByte
    } catch {
      case x: Exception => {
        stream.close
        throw x
      }
    }
  }
}
