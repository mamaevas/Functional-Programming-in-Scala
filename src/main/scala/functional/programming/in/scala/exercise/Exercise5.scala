package functional.programming.in.scala.exercise

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Exercise5 {

  sealed trait Stream[+A] {

    import Stream._

    /** Exercise 5.1
     * Write a function to convert a Stream to a List
     */
    def toList: List[A] = {
      @tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }

      go(this, List.empty[A]).reverse
    }

    def toListFast: List[A] = {
      val buf = new ListBuffer[A]

      @tailrec
      def go(s: Stream[A]): ListBuffer[A] = s match {
        case Cons(h, t) =>
          buf += h()
          go(t())
        case _ => buf
      }

      go(this).toList
    }

    /** Exercise 5.2.1
     * Write the function take(n) for returning the first n elements of a Stream
     */
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    /** Exercise 5.2.2
     * Write the function drop(n) for skipping the first n elements of a Stream
     */
    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    /** Exercise 5.3
     * Write the function takeWhile for returning all starting elements of a Stream that match the given predicate
     */
    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
      case _ => empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    /** Exercise 5.4
     * Implement forAll, which checks that all elements in the Stream match a given predicate.
     * Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
     */
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    /** Exercise 5.5
     * Use foldRight to implement takeWhile
     */
    def takeWhile2(f: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A]) { (next, acc) =>
        if (f(next)) cons(next, acc)
        else empty
      }

    /** Exercise 5.6
     * Implement headOption using foldRight
     */
    def headOption: Option[A] =
      foldRight(Option.empty[A])((next, acc) => Some(next))

    /** Exercise 5.7.1
     * Implement map using foldRight
     */
    def map[B](f: A => B): Stream[B] =
      foldRight(Stream.empty[B])((next, acc) => cons(f(next), acc))

    /** Exercise 5.7.2
     * Implement filter using foldRight */
    def filter(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A]) { (next, acc) =>
        if (p(next)) cons(next, acc)
        else acc
      }

    /** Exercise 5.7.1
     * Implement append using foldRight
     * The append method should be non-strict in its argument. */
    def append[B >: A](s: Stream[B]): Stream[B] =
      foldRight(s)((next, acc) => cons(next, acc))

    /** Exercise 5.7.1
     * Implement flatMap using foldRight
     */
    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B]) { (next, acc) =>
        f(next).append(acc)
      }

    /** Exercise 5.13.1
     * Use unfold to implement map */
    def mapUnfold[B](f: A => B): Stream[B] =
      unfold(this) {
        case Cons(h, t) => Some(f(h()), t())
        case _ => None
      }

    /** Exercise 5.13.2
     * Use unfold to implement take */
    def takeUnfold(n: Int): Stream[A] =
      unfold(this, n) {
        case (Cons(h, t), 1) => Some(h(), (empty, 0))
        case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
        case _ => None
      }

    /** Exercise 5.13.3
     * Use unfold to implement takeWhile */
    def takeWhileUnfold(f: A => Boolean): Stream[A] =
      unfold(this) {
        case Cons(h, t) if f(h()) => Some(h(), t())
        case _ => None
      }

    /** Exercise 5.13.4
     * Use unfold to implement zipWith */
    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
      unfold(this, s2) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case _ => None
      }

    def zip[B](s2: Stream[B]): Stream[(A, B)] =
      zipWith(s2)((_, _))

    /** Exercise 5.13.5
     * Use unfold to implement zipAll */
    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
      zipWithAll(s2)((_, _))

    def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      unfold(this, s2) {
        case (Empty, Empty) => None
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
        case (Cons(h1, t1), Empty) => Some(f(Some(h1()), None), (t1(), empty))
        case (Empty, Cons(h2, t2)) => Some(f(None, Some(h2())), (empty, t2()))
      }

    /** Exercise 5.14
     * Implement startsWith using functions you’ve written. It should check if one
     * Stream is a prefix of another. For instance, Stream(1,2,3) startsWith Stream(1,2)
     * would be true. */
    def startsWith[B](s2: Stream[B]): Boolean =
      zipAll(s2).takeWhile(_._2.nonEmpty).forAll(a => a._1 == a._2)

    /** Exercise 5.15
     * Implement tails using unfold.
     * For a given Stream, tails returns the Stream of suffixes of the input sequence, starting with the original Stream.
     * For example, given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()). */
    def tails: Stream[Stream[A]] =
      unfold(this) {
        case Empty => None
        case s => Some(s, s.drop(1))
      }.append(Stream(empty))

    def hasSubsequence[B](s: Stream[B]): Boolean =
      tails exists (_ startsWith s)

    /** Exercise 5.16
     * Generalize tails to the function scanRight, which is like a foldRight
     * that returns a stream of the intermediate results
     * Stream(1,2,3).scanRight(0)(_ + _).toList ==> List(6,5,3,0) */
    def scanRight[B](in: B)(f: (A, => B) => B): Stream[B] =
      foldRight((in, Stream(in)))((next, acc) => {
        lazy val _acc = acc // acc is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
        val res = f(next, _acc._1)
        (res, cons(res, _acc._2))
      })._2

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    /** Exercise 5.8
     * returns an infinite Stream of a given value */
    def constant[A](a: A): Stream[A] = {
      lazy val tail = Cons(() => a, () => constant(a))
      tail
    }

    /** Exercise 5.9
     * Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on */
    def from(n: Int): Stream[Int] = {
      lazy val tail = Cons(() => n, () => from(n + 1))
      tail
    }

    /** Exercise 5.10
     * Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on */
    def fib: Stream[Int] = {
      def go(a: Int, b: Int): Stream[Int] =
        cons(a, go(b, a + b))

      go(0, 1)
    }

    /** Exercise 5.11
     * Write a more general stream-building function called unfold. It takes an initial state,
     * and a function for producing both the next state and the next value in the generated stream. */
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((h, s)) => cons(h, unfold(s)(f))
        case None => empty
      }

    /** Exercise 5.12.1
     * Write fibs in terms of unfold */
    def fibsUnfold: Stream[Int] =
      unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

    /** Exercise 5.12.2
     * Write from in terms of unfold */
    def fromUnfold(n: Int): Stream[Int] =
      unfold(n)(a => Some(a, a + 1))

    /** Exercise 5.12.3
     * Write constant in terms of unfold */
    def constantUnfold(n: Int): Stream[Int] =
      unfold(n)(a => Some(a, a))

    /** Exercise 5.12.4
     * Write ones in terms of unfold */
    def onesUnfold: Stream[Int] =
      unfold(1)(_ => Some((1, 1)))

  }
}
