package functional.programming.in.scala.exercise

/** Exercise 4.1 */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(ob) => Some(f(ob))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(ob) => ob
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(ob => if (f(ob)) Some(ob) else None)
}

case class Some[+A](ob: A) extends Option[A]

case object None extends Option[Nothing]


object Option {
  def apply[A](ob: A): Option[A] =
    if (ob != null) Some(ob) else None

  def empty[A]: Option[A] = None

  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _ map f

  /** Exercise 4.2 */
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  /** Exercise 4.3 */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  /** Exercise 4.4
   * Write a function sequence that combines a list of Options into one Option containing
   * a list of all the Some values in the original list. If the original list contains None even
   * once, the result of the function should be None; otherwise the result should be Some
   * with a list of all the values.
   * */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

  /** Exercise 4.5 */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    List.foldLeft(a, Option(List.empty[B])) { (acc, next) =>
      map2(acc, f(next))((a, b) => List.concat(a, List(b)))
    }

}