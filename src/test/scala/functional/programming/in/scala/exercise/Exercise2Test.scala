package functional.programming.in.scala.exercise

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Exercise2Test extends AnyFlatSpec with Matchers {

    import Exercise2._

    val twoArgumentFunction = (a: Int, b: Int) => a + b
    val res1: Int = twoArgumentFunction(1, 1)
    println(s"res1 = $res1")
    assert(res1 == 2)

    "Exercise 2.3" should
        "Converts a function f of two arguments into a function of one argument that partially applies f." in {
        val curriedFunction: Int => Int => Int = curry(twoArgumentFunction)
        val res2: Int => Int = curriedFunction(1)
        val res3: Int = res2(1)
        val res4 = curriedFunction(1)(1)

        res3 shouldBe 2
        res4 shouldBe 2
    }

    "Exercise 2.4" should
        "Reverses the transformation of curry." in {
        val curriedFunction: Int => Int => Int = curry(twoArgumentFunction)
        val uncurriedFunction: (Int, Int) => Int = uncurry(curriedFunction)
        val res5 = uncurriedFunction(1, 1)

        res5 shouldBe 2
    }

    "Exercise 2.5" should
        "Implement the higher-order function that composes two functions" in {
        val fun1 = (a: Int) => 1 + a
        val fun2 = (b: Int) => b * 2
        val res6: Int => Int = compose(fun1, fun2)
        val res7: Int = res6(1)

        res7 shouldBe 3 // 1 + (1 * 2) = 3

    }
}
