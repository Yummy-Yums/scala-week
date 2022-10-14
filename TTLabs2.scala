package ttlabs

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def reduce[S](t: Tree[S])(f: (S, S) => S): S = t match {
    case Leaf(value) =>  value
    case Branch(left, right) => f(left, right)
  }

  def return_maximum[A](t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(left, right) => return_maximum(left) max return_maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left,right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(a, b) => g()
  }
}

/*
  reduce =
  if
 */

object TTLabs2 extends App {



}

