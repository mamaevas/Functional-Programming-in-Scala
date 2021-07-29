package functional.programming.in.scala.exercise

import scala.annotation.tailrec

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

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

    /**
     * Exercise 3.2
     * Implement the function tail for removing the first element of a List.
     */
    def tail[A](list: List[A]): List[A] = list match {
        case Nil => Nil
        case Cons(_, xs) => xs
    }

    /**
     * Exercise 3.3
     * implement the function setHead for replacing the first element
     * of a List with a different value
     */
    def setHead[A](list: List[A], head: A): List[A] = list match {
        case Nil => Nil
        case Cons(_, xs) => Cons(head, xs)
    }

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

    // type inference
    @tailrec
    def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
        case Cons(x, xs) if f(x) => dropWhile2(xs)(f)
        case _ => l
    }

    /**
     * Exercise 3.6
     * returns a List consisting of all but the last element of a List
     */
    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs))
    }

    /**
     * Exercise 3.9
     * Compute the length of a list using foldRight
     */
    def lengthRight[A](as: List[A]): Int =
        foldRight(as, 0)((_, acc) => acc + 1)

    /**
     * Exercise 3.10
     */
    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        }

    /**
     * Exercise 3.11
     * Write sum, product, and a function to compute the length of a list using foldLeft.
     */
    def sumF(as: List[Int]): Long = foldLeft(as, 0)(_ + _)

    def productF(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

    def lengthLeft[A](as: List[A]): Long = foldLeft(as, 0L)((acc, _) => acc + 1)

    /**
     * Exercise 3.12
     * Write a function that returns the reverse of a list (given List(1,2,3) it returns
     * List(3,2,1)). See if you can write it using a fold
     */
    def reverse[A](list: List[A]): List[A] =
        foldLeft(list, List[A]())((acc, next) => Cons(next, acc))

    /**
     * Exercise 3.13
     * Implementing foldRight via foldLeft is useful because it lets us implement
     * foldRight tail-recursively, which means it works even for large lists without overflowing the stack
     */
    def foldRightViaLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B =
        foldLeft(reverse(list), z)((acc, next) => f(next, acc))

    /**
     * Exercise 3.14
     * Implement append in terms of either foldLeft or foldRight.
     */
    def appendFoldLeft[A](list: List[A], append: A): List[A] =
        foldRightViaLeft(list, Cons(append, Nil))((next, acc) => Cons(next, acc))

    /**
     * Exercise 3.15
     * Write a function that concatenates a list of lists into a single list. Its runtime
     * should be linear in the total length of all lists. Try to use functions we have already
     * defined
     */
    def concat[A](a: List[A], b: List[A]): List[A] =
        foldRightViaLeft(a, b)((next, acc) => Cons(next, acc))

    def concat[A](a: List[List[A]]): List[A] =
        foldLeft(a, List[A]())((acc, next) => concat(acc, next))

    /**
     * Write a function that transforms a list of integers by adding 1 to each element
     */
    def appender1(a: List[Int]): List[Int] = {
        foldRightViaLeft(a, List[Int]())((next, acc) => Cons(next + 1, acc))
    }

    /**
     * Write a function that turns each value in a List[Double] into a String
     */
    def doubleToStringList(a: List[Double]): List[String] =
        foldRightViaLeft(a, List[String]())((next, acc) => Cons(next.toString, acc))

    /**
     * Write a function map that generalizes modifying each element in a list while maintaining the structure of the list.
     */
    def map[A, B](as: List[A])(f: A => B): List[B] =
        foldRightViaLeft(as, List[B]())((next, acc) => Cons(f(next), acc))

    /**
     * Write a function filter that removes elements from a list unless they satisfy a given predicate
     */
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
        foldRightViaLeft(as, List[A]()) { (next, acc) =>
            if (f(next)) Cons(next, acc)
            else acc
        }

    /**
     * Write a function flatMap that works like map except that the function given will return
     * a list instead of a single result, and that list should be inserted into the final resulting
     * list
     */
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
        foldRightViaLeft(as, List[B]())((next, acc) => concat(f(next), acc))

    /**
     * Use flatMap to implement filter
     */
    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
        flatMap(as)(a => if (f(a)) List(a) else List[A]())

    /**
     * Write a function that accepts two lists and constructs a new list by adding corresponding elements.
     * For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
     */
    def elementAddition(a: List[Int], b: List[Int]): List[Int] = {
        def elementAdditionInner(a: List[Int], b: List[Int]): (List[Int], List[Int]) =
            foldLeft(a, (List[Int](), b)) {
                case ((acc, _b), next) =>
                    _b match {
                        case Nil => (acc, Nil)
                        case Cons(h, tail) => (Cons(next + h, acc), tail)
                    }
            }

        reverse(elementAdditionInner(a, b)._1)
    }

    /**
     * Generalize previous function
     */
    def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] = {
        def elementAdditionInner(a: List[A], b: List[B]): (List[C], List[B]) =
            foldLeft(a, (List[C](), b)) {
                case ((acc, _b), next) =>
                    _b match {
                        case Nil => (acc, Nil)
                        case Cons(h, tail) => (Cons(f(next, h), acc), tail)
                    }
            }

        reverse(elementAdditionInner(a, b)._1)
    }
}