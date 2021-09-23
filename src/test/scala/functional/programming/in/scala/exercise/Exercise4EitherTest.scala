package functional.programming.in.scala.exercise

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Exercise4EitherTest extends AnyFlatSpec with Matchers {

  it should "map" in {
    Right(42).map(_ + 1) shouldBe Right(43)
    Left(42).map(_ => 1) shouldBe Left(42)
  }

  it should "flatMap" in {
    Right(42).flatMap(a => Right(a + 1)) shouldBe Right(43)
    Left(42).flatMap(a => Left(43)) shouldBe Left(42)
  }

  it should "orElse" in {
    Right(42).orElse(Right(43)) shouldBe Right(42)
    Left(42).orElse(Left(43)) shouldBe Left(43)
  }

  it should "map2" in {
    Right(42).map2(Right(1))(_ + _) shouldBe Right(43)
    Right(42).map2(Left(1))(_ + _) shouldBe Left(1)
    Left(42).map2(Right(1))((_, _) => new IllegalArgumentException) shouldBe Left(42)
    Left(42).map2(Left(42))((_, _: Nothing) => new IllegalArgumentException) shouldBe Left(42)
  }

  it should "sequence" in {
    import Either._
    sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
    sequence(List(Right(1), Left(2), Right(3))) shouldBe Left(2)
    sequence(List(Left(1), Left(2), Left(3))) shouldBe Left(1)
  }

  it should "traverse" in {
    import Either._
    traverse(List(1, 2, 3))(a => Right(a)) shouldBe Right(List(1, 2, 3))
    traverse(List(1, 2, 3))(a => if (a % 2 == 0) Left(2) else Right(a)) shouldBe Left(2)
  }
}
