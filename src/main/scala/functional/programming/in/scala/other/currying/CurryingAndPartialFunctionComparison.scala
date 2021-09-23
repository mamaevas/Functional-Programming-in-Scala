package functional.programming.in.scala.other.currying

import scala.annotation.tailrec


object CurryingAndPartialFunctionComparison {
  // Каррирование — преобразование функции от многих аргументов в набор функций, каждая из которых является функцией от одного аргумента.

  // Function
  val sum: (Int, Int) => Int = (x, y) => x + y
  sum(1, 2) == 3

  // Curried
  val curriedSum: Int => Int => Int = sum.curried
  val curriedSum_2: Int => Int => Int = x => y => x + y
  curriedSum(1)(2) == 3

  // Partial function, partial application.. the same result
  val pSum: Int => Int = sum(1, _)
  val curriedSum_3: Int => Int = curriedSum(1)


  def sum3(x: Int, y: Int, z: Int): Int = x + y + z

  sum3(1, 2, _) // Int => Int = $Lambda
  sum3(1, _, _) // (Int, Int) => Int = $Lambda
  sum3 _ // Int, Int, Int) => Int = $Lambda

  def curriedSum3(x: Int)(y: Int)(z: Int): Int = x + y + z

  curriedSum3(1)(2) _ // Int => Int = $Lambda
  curriedSum3(1) _ // Int => (Int => Int) = $Lambda
  curriedSum3 _ // Int => (Int => (Int => Int)) = $Lambda


  // Function find
  @tailrec
  def find[A](xs: List[A], predicate: A => Boolean): Option[A] =
    xs match {
      case Nil => None
      case head :: tail =>
        if (predicate(head)) Some(head) else find(tail, predicate)
    }

  // Type inference takes into account only one parameter list at the time.
  // So, we should define type of x
  find(List(1, 2, 3), (x: Int) => x % 2 == 0) == Some(2)

  // Curried find
  @tailrec
  def findCurried[A](xs: List[A])(predicate: A => Boolean): Option[A] =
    xs match {
      case Nil => None
      case head :: tail =>
        if (predicate(head)) Some(head) else findCurried(tail)(predicate)
    }

  // This tiny change will help the compiler resolve the type properly.
  findCurried(List(1, 2, 3))(x => x % 2 == 0) == Some(2)


  val numbers: List[Int] = List(1, 2, 3)
  // Function sum
  val summ: (Int, Int) => Int = (x, y) => x + y
  // Function
  numbers.map(n => summ(1, n)) == List(2, 3, 4)
  // Partial function
  numbers.map(summ(1, _)) == List(2, 3, 4)
  numbers.map(summ.curried(1)) == List(2, 3, 4)

  // Curried sum
  val curriedSumm: Int => Int => Int = x => y => x + y
  numbers.map(curriedSumm(1)) == List(2, 3, 4)

}
