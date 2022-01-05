package functional.programming.in.scala.exercise

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Exercise5Test extends AnyFlatSpec with Matchers {

  import Exercise5._

  it should "toList" in {
    Stream(1, 2, 3, 4, 5).toList shouldBe List(1, 2, 3, 4, 5)
    Stream.empty[Int].toList shouldBe List.empty[Int]
  }

  it should "toListFast" in {
    Stream(1, 2, 3, 4, 5).toListFast shouldBe List(1, 2, 3, 4, 5)
  }

  it should "foldRight" in {
    Stream(1, 2, 3, 4, 5).foldRight(List.empty[Int])((next, acc) => acc :+ next) shouldBe List(5, 4, 3, 2, 1)
  }

  it should "take" in {
    Stream(1, 2, 3, 4, 5).take(2).toList shouldBe List(1, 2)
  }

  it should "drop" in {
    Stream(1, 2, 3, 4, 5).drop(2).toList shouldBe List(3, 4, 5)
  }

  it should "takeWhile" in {
    Stream(1, 2, 3, 4, 5).takeWhile((n: Int) => n % 3 != 0).toList shouldBe List(1, 2)
  }

  it should "forAll" in {
    Stream(1, 2, 3, 4, 5).forAll(_ < 10) shouldBe true
    Stream(1, 2, 3, 4, 5).forAll(_ % 2 == 0) shouldBe false
  }

  it should "takeWhile2" in {
    Stream(1, 2, 3, 4, 5).takeWhile2((n: Int) => n % 3 != 0).toList shouldBe List(1, 2)
  }

  it should "headOption" in {
    Stream(1, 2, 3, 4, 5).headOption shouldBe Some(1)
    Stream.empty[Int].headOption shouldBe None
  }

  it should "map" in {
    Stream(1, 2, 3, 4, 5).map(_ * 2).toList shouldBe List(2, 4, 6, 8, 10)
  }

  it should "filter" in {
    Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList shouldBe List(2, 4)
  }

  it should "append" in {
    Stream(1, 2, 3, 4, 5).append(Stream(6, 7, 8)).toList shouldBe List(1, 2, 3, 4, 5, 6, 7, 8)
  }

  it should "flatMap" in {
    Stream(1, 2, 3, 4, 5).flatMap(a => Stream(a * 2)).toList shouldBe List(2, 4, 6, 8, 10)
  }

  it should "constant" in {
    Stream.constant(3).drop(3).take(2).toList shouldBe List(3, 3)
  }

  it should "from" in {
    Stream.from(3).take(2).toList shouldBe List(3, 4)
  }

  it should "generate fibonacci" in {
    Stream.fib.take(10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  }

  it should "unfold" in {
    Stream.unfold(1)(a => Some((a, a + 1)).filterNot(_._1 % 5 == 0)).toList shouldBe List(1, 2, 3, 4)
  }

  it should "generate fibonacci via unfold" in {
    Stream.fibsUnfold.take(10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  }

  it should "from via unfold" in {
    Stream.fromUnfold(3).take(2).toList shouldBe List(3, 4)
  }

  it should "constant via unfold" in {
    Stream.constantUnfold(3).drop(3).take(2).toList shouldBe List(3, 3)
  }

  it should "ones via unfold" in {
    Stream.onesUnfold.drop(3).take(2).toList shouldBe List(1, 1)
  }

  it should "map via unfold" in {
    Stream(1, 2, 3, 4, 5).mapUnfold(_ * 2).toList shouldBe List(2, 4, 6, 8, 10)
  }

  it should "take via unfold" in {
    Stream(1, 2, 3, 4, 5).takeUnfold(2).toList shouldBe List(1, 2)
  }

  it should "takeWhile via unfold" in {
    Stream(1, 2, 3, 4, 5).takeWhileUnfold((n: Int) => n % 3 != 0).toList shouldBe List(1, 2)
  }

  it should "zipWith via unfold" in {
    Stream(1, 2, 3).zipWith(Stream(-1, -2, -3))((_, _)).toList shouldBe List((1, -1), (2, -2), (3, -3))
  }

  it should "zipWithAll via unfold" in {
    Stream(1, 2).zipWithAll(Stream(-1, -2, -3))((_, _)).toList shouldBe List((Some(1), Some(-1)), (Some(2), Some(-2)), (None, Some(-3)))
    Stream(1, 2, 3).zipWithAll(Stream(-1, -2))((_, _)).toList shouldBe List((Some(1), Some(-1)), (Some(2), Some(-2)), (Some(3), None))
  }

  it should "startWith" in {
    Stream(1, 2, 3).startsWith(Stream(1, 2)) shouldBe true
    Stream(1, 3, 2).startsWith(Stream(1, 2)) shouldBe false
    Stream(1).startsWith(Stream(1, 2)) shouldBe false
  }

  it should "tails" in {
    Stream(1, 2, 3).tails.toList.map(_.toList) shouldBe List(List(1, 2, 3), List(2, 3), List(3), List.empty[Int])
  }

  it should "scanRight" in {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
  }
}
