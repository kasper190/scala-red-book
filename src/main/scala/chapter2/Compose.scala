package chapter2

object Compose {
  // Exercise 2.5: Compose
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
