package ch.awae.wasm.ast

import ch.awae.wasm.ast.NumericValue.I32
import org.scalatest._

class NumericSpec extends FlatSpec with Matchers {

  val bytes = List(0xE5.toByte,0x8E.toByte,0x26.toByte)

  "An Integer" should "be converted to a Byte Stream correctly" in {
    assertResult(bytes)(I32(624485).bytes)
  }

  "A Byte Stream" should "produce the correct Integer" in {
    assertResult(624485)(I32(bytes).unsigned)
  }
}