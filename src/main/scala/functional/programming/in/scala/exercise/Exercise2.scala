package functional.programming.in.scala.exercise

object Exercise2 extends App {

    val twoArgumentFunction = (a: Int, b: Int) => a + b
    val res1: Int = twoArgumentFunction(1, 1)
    println(s"res1 = $res1")
    assert(res1 == 2)

    /**
     * Exercise 2.3
     * Converts a function f of two arguments into a function of one argument that partially applies f. */
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => f(a, _: B)

    val curriedFunction: Int => Int => Int = curry(twoArgumentFunction)
    val res2: Int => Int = curriedFunction(1)
    val res3: Int = res2(1)
    val res4 = curriedFunction(1)(1)
    println(s"res3 = $res3")
    assert(res3 == 2)
    println(s"res4 = $res4")
    assert(res4 == 2)

    /**
     * Exercise 2.4
     * Reverses the transformation of curry. */
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

    val uncurriedFunction: (Int, Int) => Int = uncurry(curriedFunction)
    val res5 = uncurriedFunction(1, 1)
    println(s"res5 = $res5")
    assert(res5 == 2)

    /**
     * Exercise 2.5
     * Implement the higher-order function that composes two functions */
    def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

    val fun1 = (a: Int) => 1 + a
    val fun2 = (b: Int) => b * 2
    val res6: Int => Int = compose(fun1, fun2)
    val res7: Int = res6(1)
    println(s"res7 = $res7") // 1 + (1 * 2) = 3
    assert(res7 == 3)
}
