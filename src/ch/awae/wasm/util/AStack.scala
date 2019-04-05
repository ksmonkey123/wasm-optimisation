package ch.awae.wasm.util

class AStack[T] private(private[this] var stack: List[T]) {

  def head: T = stack.head

  def push(t: T): Unit = stack = t :: stack

  def pop(): T = pop(1).head

  def pop(n: Int): List[T] = {
    val res = stack take n
    stack = stack drop n
    res
  }

  def toList: List[T] = stack

}

object AStack {
  def apply[T](stack: List[T]) = new AStack(stack)

  def empty[T] = new AStack(List.empty[T])
}
