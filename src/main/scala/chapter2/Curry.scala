package chapter2

object Curry {
  // Exercise 2.3: Curry
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // Exercise 2.4: UnCurry
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)
}
