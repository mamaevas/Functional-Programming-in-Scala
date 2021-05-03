package functional.programming.in.scala.exercise

object Exercise2 {

    /**
     * Exercise 2.3
     * Converts a function f of two arguments into a function of one argument that partially applies f. */
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => f(a, _: B)

    /**
     * Exercise 2.4
     * Reverses the transformation of curry. */
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

    /**
     * Exercise 2.5
     * Implement the higher-order function that composes two functions */
    def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
