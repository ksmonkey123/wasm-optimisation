package ch.awae.wasm.io

import org.scalatest._

class DataStreamSpec extends FlatSpec with Matchers {

  "An empty stream" should "not produce values" in {
    assert(DataStream.ofList(Nil).takeOptional.isEmpty)
  }

  it should "throw an exception if accessed" in {
    assertThrows[NoSuchElementException](DataStream.ofList(Nil).take)
  }

  it should "be prependable" in {
    val stream = 1.toByte :: DataStream.ofList(Nil)
    assertResult(1)(stream.take)
  }

  "A non-empty stream" should "produce the values in correct order" in {
    val stream = DataStream.ofList(List(1,2,3,4,5))
    for (     i <- 1 to 5) {
      assertResult(i)(stream.take)
    }
  }

  it should "produce multiple values in the correct order" in {
    val stream = DataStream.ofList(List(1,2,3,4,5))
    assertResult(List(1, 2, 3, 4, 5))(stream take 5)
  }

  it should "throw an exception if accessed beyond its end" in {
    val stream = DataStream.ofList(List(1, 2,3,4,5))
    assertThrows[NoSuchElementException](stream take 6)
  }

  it should "be prependable" in {
    val stream = DataStream.ofList(List(2, 3, 4, 5))
    assertResult(2)(stream.take)
    1.toByte :: stream
    assertResult(List(1,3,4,5))(stream take 4)
  }

}
