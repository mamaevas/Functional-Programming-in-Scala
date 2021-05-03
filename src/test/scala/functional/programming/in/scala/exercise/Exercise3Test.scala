package functional.programming.in.scala.exercise

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Exercise3Test extends AnyFlatSpec with Matchers {

    import List._

    "Exercise 3.1" should
        "What will be the result of the following match expression?" in {
        val res3_1 = List(1, 2, 3, 4, 5) match {
            case Cons(x, Cons(2, Cons(4, _))) => x
            case Nil => 42
            case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // <= that case x + y = 1 + 2 = 3
            case Cons(h, t) => h + sum(t)
            case _ => 101
        }
        res3_1 shouldBe 3
    }

    "Exercise 3.2" should
        "Implement the function tail for removing the first element of a List" in {
        val testList = List(1, 2, 3)
        val res3_2 = tail(testList)
        res3_2 shouldBe Cons(2, Cons(3, Nil))
    }

    "Exercise 3.3" should
        "implement the function setHead for replacing the first element of a List with a different value" in {
        val testList = List(1, 2, 3)
        val res3_3 = setHead(testList, 25)
        res3_3 shouldBe Cons(25, Cons(2, Cons(3, Nil)))
    }

    "Exercise 3.4" should
        "Generalize tail to the function drop, which removes the first n elements from a list" in {
        val testList = List(1, 2, 3)
        val res3_4 = drop(testList, 2)
        res3_4 shouldBe Cons(3, Nil)
    }

    "Exercise 3.5" should
        "removes elements from the List prefix as long as they match a predicate" in {
        val testList = List(1, 2, 3)
        val res3_5 = dropWhile(testList, (_: Int) == 2)
        val res3_5_0 = dropWhile(testList, (x: Int) => x == 2) // Can do like this
        //    val res3_5 = dropWhile(testList, x => x == 2) Cannot do like this
        res3_5 shouldBe Cons(2, Cons(3, Nil))

        val res3_5_2 = dropWhile2(testList)(_ == 2)
        val res3_5_2_1 = dropWhile2(testList)((x: Int) => x == 2) // Can do like this
        val res3_5_2_2 = dropWhile2(testList)(x => x == 2) // Also Can do like this
        res3_5 shouldBe Cons(2, Cons(3, Nil))
    }

    "Exercise 3.6" should
        "returns a List consisting of all but the last element of a List" in {
        val testList = List(1, 2, 3)
        val res3_6 = init(testList)
        res3_6 shouldBe Cons(1, Cons(2, Nil))
    }

    "Exercise 3.9" should
        "Compute the length of a list using foldRight" in {
        val res3_9 = lengthRight(List(1, 2, 3, 4))
        res3_9 shouldBe 4
    }

    "Exercise 3.10" should "be fine" in {
        val res3_10 = foldLeft(List(1, 2, 3), 4)(_ + _)
        res3_10 shouldBe 10
    }

    "Exercise 3.11" should
        "Write sum, product, and a function to compute the length of a list using foldLeft." in {
        val res3_11_1 = sumF(List(1, 2, 3, 4))
        res3_11_1 shouldBe 10

        val res3_11_2 = productF(List(1, 2, 3, 4))
        res3_11_2 shouldBe 24

        val res3_11_3 = lengthLeft(List(1, 2, 3, 4))
        res3_11_3 shouldBe 4
    }

    "Exercise 3.12" should "" +
        "Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1))." +
        "See if you can write it using a fold" in {
        val res3_12_1 = reverse(List(1, 2, 3, 4))
        res3_12_1 shouldBe List(4, 3, 2, 1)
    }
}