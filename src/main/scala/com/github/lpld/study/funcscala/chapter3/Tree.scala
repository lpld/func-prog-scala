package com.github.lpld.study.funcscala.chapter3

/**
  * @author leopold
  * @since 25/08/16
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /*
   3.25
   */
  def size(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  /*
   3.26
   */
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  /*
   3.27
   */
  def depth(tree: Tree[Int]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => depth(left) max depth(right) + 1
  }

  /*
   3.28
   */
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  /*
   3.29
   */
  def fold[A, B](tree: Tree[A])(onLeaf: A => B)(onBranch: (B, B) => B): B = tree match {

    case Leaf(value) => onLeaf(value)
    case Branch(left, right) => onBranch(
      fold(left)(onLeaf)(onBranch),
      fold(right)(onLeaf)(onBranch)
    )
  }

  def sizeViaFold[A](tree: Tree[_]) = fold(tree)(_ => 1)(_ + _ + 1)

  def maxViaFold(tree: Tree[Int]) = fold(tree)(identity)(_ max _)

  def depthViaFold(tree: Tree[_]) = fold(tree)(_ => 0)(_ max _ + 1)

  def mapViaFold[A, B](tree: Tree[A])(f: A => B) = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
