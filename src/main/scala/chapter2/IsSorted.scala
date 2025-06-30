package chapter2

object IsSorted {
  // Exercise 2.2: IsSorted
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if n >= as.length - 1 then true
      else if !ordered(as(n), as(n + 1)) then false
      else loop(n + 1)

    loop(0)
  }
}