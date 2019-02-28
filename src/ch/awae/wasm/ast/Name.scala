package ch.awae.wasm.ast

import java.nio.charset.StandardCharsets

case class Name(name: String)

object Name {
  def apply(stream: DataStream): Name = Name(new String(Vec(stream, _.take).elems.toArray, StandardCharsets.UTF_8))
}