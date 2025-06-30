import chapter2.Compose.compose
import chapter2.Curry.curry
import chapter2.Curry.uncurry
import chapter2.Fibonacci.fib
import chapter2.IsSorted.isSorted

class Chapter2Suite extends munit.FunSuite {
  test("2.1 Fibonacci") {
    val result = fib(5)
    assertEquals(result, 5)
  }
  
  test("2.2 IsSorted") {
    val result1 = isSorted(Array(1, 2, 3), _ < _)
    assertEquals(result1, true)

    val result2 = isSorted(Array(1, 2, 1), _ > _)
    assertEquals(result2, false)

    val result3 = isSorted(Array(3, 2, 1), _ < _)
    assertEquals(result3, false)

    val result4 = isSorted(Array(1, 2, 3), _ < _)
    assertEquals(result4, true)
  }

  test("2.3 Curry") {
    val add = (x: Int, y: Int) => x + y
    val curryAdd = curry(add)
    val result = curryAdd(1)(2)
    assertEquals(result, 3)
  }

  test("2.4 UnCurry") {
    val curryAdd = (x: Int) => (y: Int) => x + y
    val unCurryAdd = uncurry(curryAdd)
    val result = unCurryAdd(1, 2)
    assertEquals(result, 3)
  }

  test("2.5 Compose") {
    val addOne: Int => Int = _ + 1
    val double: Int => Int = _ * 2
    val composed = compose(double, addOne)
    val result = composed(3)
    assertEquals(result, 8) // (3 + 1) * 2 = 8
  }
}
