package functional.programming.in.scala.exercise

import scala.annotation.tailrec

object Exercise3 extends App {

    /**
     * Exercise 3.1
     * What will be the result of the following match expression?
     */

    import List._

    val res3_1 = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // <= that case x + y = 1 + 2 = 3
        case Cons(h, t) => h + sum(t)
        case _ => 101
    }

    println(s"res3_1 = $res3_1")
    assert(res3_1 == 3)


    /**
     * Exercise 3.2
     * Implement the function tail for removing the first element of a List.
     */

    def tail[A](list: List[A]) = list match {
        case Nil => Nil
        case Cons(_, xs) => xs
    }

    val testList = List(1, 2, 3)
    val res3_2 = tail(testList)
    println(s"res3_2 = $res3_2")
    assert(res3_2 == Cons(2,Cons(3, Nil)))

    /**
     * Exercise 3.3
     * implement the function setHead for replacing the first element
     * of a List with a different value
     */

    def setHead[A](list: List[A], head: A): List[A] = list match {
        case Nil => Nil
        case Cons(_, xs) => Cons(head, xs)
    }

    val res3_3 = setHead(testList, 25)
    println(s"res3_3 = $res3_3")
    assert(res3_3 == Cons(25,Cons(2,Cons(3,Nil))))

    /**
     * Exercise 3.4
     * Generalize tail to the function drop, which removes the first n elements from a list.
     */
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] =
        if (n <= 0) l
        else l match {
            case Nil => Nil
            case Cons(_, xs) => drop(xs, n - 1)
        }

    val res3_4 = drop(testList, 2)
    println(s"res3_4 = $res3_4")
    assert(res3_4 == Cons(3,Nil))

    /**
     * Exercise 3.5
     * removes elements from the List prefix as long as they match a predicate
     */
    @tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(x, xs) =>
            if (f(x)) Cons(x, xs)
            else dropWhile(xs, f)
    }

    val res3_5 = dropWhile(testList, (_: Int) == 2)
    println(s"res3_5 = $res3_5")
    assert(res3_5 == Cons(2,Cons(3,Nil)))

    /**
     * Exercise 3.6
     * returns a List consisting of all but the last element of a List
     */
    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs))
    }

    val res3_6 = init(testList)
    println(s"res3_6 = $res3_6")
    assert(res3_6 == Cons(1,Cons(2,Nil)))
}


sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}