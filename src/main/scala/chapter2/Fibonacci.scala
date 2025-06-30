package chapter2

object Fibonacci {
  // Exercise 2.1: Fibonacci
  def fib(n: Int): Int =
    @annotation.tailrec
    def go(n: Int, curr: Int, next: Int): Int =
      if n <= 0 then curr
      else go(n - 1, next, curr + next)
    
    go(n, 0, 1)
}
