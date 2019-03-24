package ch.awae.wasm.util

import java.util.concurrent.atomic.AtomicInteger

trait Indexer[+T] {
  def next : T
}

class SequentialIndexer(start : Int) extends Indexer[Int] {

  private[this] val atomic = new AtomicInteger(start)

  def next : Int = atomic.getAndIncrement()

}