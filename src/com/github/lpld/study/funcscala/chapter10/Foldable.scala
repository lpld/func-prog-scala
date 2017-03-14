package com.github.lpld.study.funcscala.chapter10

import com.github.lpld.study.funcscala.chapter3.{Branch, Leaf, Tree}

/**
  * @author leopold
  * @since 13/03/17
  */
trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  /*
   * 10.15. Write a function that converts any Foldable into a List
   */
  def toList[A](as: F[A]): List[A] =
    foldRight(as)(Nil: List[A])(_ :: _)
}

object Foldable {
  /*
   * 10.12. Implement `Foldable[List]`, `Foldable[IndexedSeq]` and `Foldable[Stream]`.
   */
  val listFoldable = new Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  val indexedSeqFoldable = new Foldable[IndexedSeq] {
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B =
      Monoid.foldMapV(as, mb)(f)
  }

  val stramFoldable = new Foldable[Stream] {
    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  /*
   * 10.13. Implement `Foldable[Tree]`.
   */
  val treeFoldable = new Foldable[Tree] {
    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(a) => f(a, z)
      case Branch(left, right) =>
        val rightRes = foldRight(right)(z)(f)
        foldRight(left)(rightRes)(f)
    }

    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(a) => f(z, a)
      case Branch(left, right) =>
        val leftRes = foldLeft(left)(z)(f)
        foldLeft(right)(leftRes)(f)
    }
  }

  /*
   * 10.14. Write a `Foldable[Option]` instance.
   */
  val optionFoldable = new Foldable[Option] {
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as.map(a => f(a, z)).getOrElse(z)

    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      foldRight(as)(z)((b, a) => f(a, b))
  }
}
