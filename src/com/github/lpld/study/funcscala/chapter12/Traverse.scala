package com.github.lpld.study.funcscala.chapter12

import com.github.lpld.study.funcscala.chapter10.{Foldable, Monoid}
import com.github.lpld.study.funcscala.chapter11.{Functor, Id, Monad}
import com.github.lpld.study.funcscala.chapter6.State
import com.github.lpld.study.funcscala.chapter6.State._

import scala.language.reflectiveCalls

/**
  * @author leopold
  * @since 16/03/17
  */
trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>

  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence[G, B](map(fa)(f))

  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(identity)

  /*
   * 12.14. Implement `map` in terms of traverse.
   */
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(a => Id(f(a)))(Id.idMonad).value

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

  type Const[M, B] = M

  def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero

      override def map2[A, B, C](ma: M, mb: M)(f: (A, B) => C): M = M.op(ma, mb)
    }

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) => for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i)).run(0)._1

  override def toList[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => for {
      as <- get[List[A]]
      _ <- set(a :: as)
    } yield ()).run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      st <- get[S]
      (b, newSt) = f(a, st)
      _ <- set(newSt)
    } yield b).run(s)

  def toList2[A](fa: F[A]): List[A] =
    mapAccum(fa, List.empty[A])((a, as) => ((), a :: as))._2.reverse

  def zipWithIndex2[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, i) => ((a, i), i + 1))._1

  /*
   * 12.16. Write `reverse` function for traversables.
   */
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, s) => (s.head, s.tail))._1

  /*
   * 12.17. Use `mapAccum` to give default implementation of `foldLeft` for the `Traverse` trait.
   */
  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(as, z)((a, s) => ((), f(s, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (_, Nil) => sys.error("zip: Incompatible shapes")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  /*
   * 12.18. Use applicative functors products to write the fusion of two traversals.
   */
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  /*
   * 12.19. Implement the composition of two `Traversable` instances.
   */
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[H[_]: Applicative, A, B](fa: F[G[A]])(f: (A) => H[B]): H[F[G[B]]] =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }
}

object Traverse {
  /*
   * 12.13. Write `Traverse` instances for `List`, `Option` and `Tree`.
   */
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List.empty[B]))((a, fla) => G.map2(f(a), fla)(_ :: _))
  }

  def mapTraverse[K] = new Traverse[({type f[x] = Map[K, x]})#f] {
    override def traverse[G[_], A, B](fa: Map[K, A])(f: (A) => G[B])(implicit G: Applicative[G]): G[Map[K, B]] =
      fa.foldRight(G.unit(Map.empty[K, B])) { case ((k, fv), fm) => G.map2(f(fv), fm)((v, map) => map + (k -> v)) }
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case None => G.unit(None)
        case Some(a) => G.map(f(a))(Some(_))
      }
  }

  val treeTraverse = ???
}
