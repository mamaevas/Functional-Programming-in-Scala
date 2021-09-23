package functional.programming.in.scala.exercise

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Exercise3TreeTest extends AnyFlatSpec with Matchers {

  it should "size" in {
    val testTree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    Tree.size(testTree) shouldBe 7
  }

  it should "max" in {
    val testTree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    Tree.max(testTree) shouldBe 4
  }

  it should "depth" in {
    val testTree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    Tree.depth(testTree) shouldBe 2
  }

  it should "map" in {
    val testTree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    val newTree: Tree[Int] = Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8)))
    Tree.map(testTree)(_ * 2) shouldBe newTree
  }

  it should "fold" in {
    val testTree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    Tree.fold(testTree)(_ * 2)(_ + _) shouldBe 20
  }
}
