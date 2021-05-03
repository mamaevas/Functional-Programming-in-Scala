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
    def reverse[A](list: List[A]): List[A] = {
        foldLeft(list, List[A]())((acc, next) => Cons(next, acc))
    }

}