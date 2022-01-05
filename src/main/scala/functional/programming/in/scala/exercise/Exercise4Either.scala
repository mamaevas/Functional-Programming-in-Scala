package functional.programming.in.scala.exercise


object Exercise4Either {

  import Exercise3List._

  /** Exercise 4.6 */
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] =
      this match {
        case Right(value) => Right(f(value))
        case v: Left[E] => v
      }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case Right(value) => f(value)
        case v: Left[E] => v
      }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
      this match {
        case v: Right[A] => v
        case Left(_) => b
      }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        _a <- this
        _b <- b
      } yield f(_a, _b)
  }

  object Either {

    /** Exercise 4.7 */
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      traverse(es)(identity)

    /** Exercise 4.7 */
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      List.foldLeft[A, Either[E, List[B]]](as, Right(List.empty[B])) { (acc, next) =>
        acc match {
          case v: Right[List[A]] => v.map2(f(next))((a, b) => List.concat(a, List(b)))
          case e => e
        }
      }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

}