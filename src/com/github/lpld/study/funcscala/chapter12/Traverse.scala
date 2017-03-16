package com.github.lpld.study.funcscala.chapter12

import com.github.lpld.study.funcscala.chapter11.Id

import scala.language.reflectiveCalls

/**
  * @author leopold
  * @since 16/03/17
  */
trait Traverse[F[_]] {
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence[G, B](map(fa)(f))

  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(identity)

  /*
   * 12.14. Implement `map` in terms of traverse.
   */
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(a => Id(f(a)))(Id.idMonad).value
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
