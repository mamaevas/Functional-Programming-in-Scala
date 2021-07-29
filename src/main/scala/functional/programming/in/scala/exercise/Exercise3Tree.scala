package functional.programming.in.scala.exercise

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

    /** Exercise 3.25
     * Write a function size that counts the number of nodes (leaves and branches) in a tree.
     */
    def size[A](t: Tree[A]): Int =
        t match {
            case Leaf(value) => 1
            case Branch(left, right) => size(left) + size(right) + 1
        }

    /** Exercise 3.26
     * Write a function maximum that returns the maximum element in a Tree[Int]
     */
    def max(t: Tree[Int]): Int =
        t match {
            case Leaf(value) => value
            case Branch(left, right) => max(left) max max(right)
        }

    /** Exercise 3.27
     * Write a function depth that returns the maximum path length from the root of a tree to any leaf
     */
    def depth[A](t: Tree[A]): Int =
        t match {
            case Leaf(value) => 0
            case Branch(left, right) => (depth(left) max depth(right)) + 1
        }

    /** Exercise 3.28
     * Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with a given function.
     */
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
        t match {
            case Leaf(value) => Leaf(f(value))
            case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        }

    /** Exercise 3.29 */
    def fold[A, B](t: Tree[A])(f: A => B)(acc: (B, B) => B): B =
        t match {
            case Leaf(value) => f(value)
            case Branch(left, right) => acc(fold(left)(f)(acc), fold(right)(f)(acc))
        }
}

