package ch.awae.wasm.ast

import ch.awae.wasm.ast.NumericValue.I32
import org.scalatest._

class NumericSpec extends FlatSpec with Matchers {

  "An Integer" should "be converted to a Byte Stream correctly" in {
    assertResult(List(0xe5, 0x8e, 0x26).map(_.toByte))(I32(624485).bytes)
  }

  "A Byte Stream" should "produce the correct Integer" in {
    assertResult(624485)(I32(List(0xe5, 0x8e, 0x26).map(_.toByte)).unsigned)
  }

}