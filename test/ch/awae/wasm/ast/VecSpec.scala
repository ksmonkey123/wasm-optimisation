package ch.awae.wasm.ast

import ch.awae.wasm.io.DataStream
import org.scalatest._

class VecSpec extends FlatSpec with Matchers {

  "Vec" should "parse the correct number of elements" in {
    val stream = DataStream.ofList(List(3, 1, 2, 3).map(_.toByte))
    assertResult(List(1, 2, 3))(Vec(stream, _.take.toInt))
  }

  it should "return an empty list if size is zero" in {
    val stream = DataStream.ofList(List(0, 1, 2, 3).map(_.toByte))
    assertResult(Nil)(Vec(stream, _.take))
    assertResult(1)(stream.take)
  }

  it should "be able to parse an empty list" in {
    assertResult(0x00.toByte::Nil)(Vec.compile(Nil))
  }

  it should "properly parse a non-empty list" in {
    assertResult(List(3, 1, 2, 3).map(_.toByte)) {
      Vec.compile(List(1, 2, 3), (_: Int).toByte :: Nil)
    }
  }

}
