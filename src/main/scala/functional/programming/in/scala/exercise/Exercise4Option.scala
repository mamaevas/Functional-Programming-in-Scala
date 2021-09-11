package functional.programming.in.scala.exercise

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

    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        for {
            aa <- a
            bb <- b
        } yield f(aa, bb)
}

object Exercises4 {

    def variance(xs: Seq[Double]): Option[Double] = {
        def mean(xs: Seq[Double]): Option[Double] =
            if (xs.isEmpty) None
            else Some(xs.sum / xs.length)

        mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }
}