package functional.programming.in.scala.exercise

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Exercise4OptionTest extends AnyFlatSpec with Matchers {

  it should "apply" in {
    Option(null) shouldBe None
    Option(42) shouldBe Some(42)
  }

  it should "map" in {
    Some(42).map(_ => 1) shouldBe Some(1)
    None.map(_ => 1) shouldBe None
  }

  it should "flatMap" in {
    Some(42).flatMap(_ => Some(1)) shouldBe Some(1)
    Some(42).flatMap(_ => None) shouldBe None
    None.flatMap(_ => Some(1)) shouldBe None
    None.flatMap(_ => None) shouldBe None
  }

  it should "getOrElse" in {
    Some(42).getOrElse(1) shouldBe 42
    None.getOrElse(1) shouldBe 1
  }

  it should "orElse" in {
    Some(42).orElse(Some(1)) shouldBe Some(42)
    Some(42).orElse(None) shouldBe Some(42)
    None.orElse(Some(1)) shouldBe Some(1)
    None.orElse(None) shouldBe None
  }

  it should "filter" in {
    Some(42).filter(_ => true) shouldBe Some(42)
    Some(42).filter(_ => false) shouldBe None
    None.filter(_ => true) shouldBe None
    None.filter(_ => false) shouldBe None
  }

  it should "variance" in {
    import Option._
    variance(Seq(1, 2, 3, 4, 5)) shouldBe Some(2)
    variance(Seq()) shouldBe None
  }

  it should "map2" in {
    import Option._
    map2(Some(42), Some(1))(_ + _) shouldBe Some(43)
    map2(Some(42), None)(_ + _) shouldBe None
    map2(Option.empty[Int], Some(42))(_ + _) shouldBe None
    map2(Option.empty[Int], Option.empty[Int])(_ + _) shouldBe None
  }

  it should "sequence" in {
    import Option._
    sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    sequence(List(Some(1), None, Some(3))) shouldBe None
    sequence(List(None, None, None)) shouldBe None
  }

  it should "traverse" in {
    import Option._
    traverse(List(1, 2, 3))(a => Some(a)) shouldBe Some(List(1, 2, 3))
    traverse(List(1, 2, 3))(a => if (a % 2 == 0) None else Some(a)) shouldBe None
    traverse(List.empty[Int])(a => Some(a)) shouldBe Some(List.empty[Int])
  }


}
