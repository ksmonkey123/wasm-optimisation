package ch.awae.wasm.ast

import java.nio.charset.StandardCharsets

object Name {
  private[ast] def apply(stream: DataStream) = new String(Vec(stream, _.take).toArray, StandardCharsets.UTF_8)
}