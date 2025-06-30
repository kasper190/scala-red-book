package chapter3

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])


object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))
  
  def sum(ints: List[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  
  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = as match
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _)

  // Exercise 3.2: Tail
  def tail[A](l: List[A]): List[A] = l match
    case Nil => sys.error("Empty list")
    case Cons(x, xs) => xs
  
  // Exercise 3.3: SetHead
  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => sys.error("Empty list")
    case Cons(_, xs) => Cons(h, xs)
  
  // Exercise 3.4: Drop
  def drop[A](as: List[A], n: Int): List[A] =
    if n <= 0 then as
    else as match
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
  
  // Exercise 3.5: DropWhile
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => as

  // Exercise 3.6: Init
  def init[A](as: List[A]): List[A] = as match
    case Nil => sys.error("Empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  
  // Exercise 3.9: Length
  def length[A](as: List[A]): Int =
    foldRight(as, 0, (_, acc) => acc + 1)
  
  // Exercise 3.10: FoldLeft
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = as match
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f)
  
  // Exercise 3.11
  def sumLeft(l: List[Int]): Int =
    foldLeft(l, 0, _ + _)
  
  // Exercise 3.11
  def productLeft(l: List[Double]): Double =
    foldLeft(l, 1.0, _ * _)

  // Exercise 3.11
  def lengthLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (acc, _) => acc + 1)
  
  // Exercise 3.12
  def reverseFold[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A], (acc, a) => Cons(a, acc))
  
  // Exercise 3.13
  def foldRightOnFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverseFold(as), acc, (b, a) => f(a, b))
  
  // Exercise 3.14
  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2, Cons(_, _))
  
  // Exercise 3.15
  // TODO

  // Exercise 3.16
  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int], (i, acc) => Cons(i + 1, acc))
  
  // Exercise 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String], (a, acc) => Cons(a.toString, acc))