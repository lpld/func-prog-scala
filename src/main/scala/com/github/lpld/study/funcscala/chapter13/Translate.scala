package com.github.lpld.study.funcscala.chapter13

import scala.language.higherKinds

/**
  * @author leopold
  * @since 19/09/18
  */
trait Translate[F[_], G[_]] {def apply[A](f: F[A]): G[A] }
object Translate {

  type ~>[F[_], G[_]] = Translate[F, G]
}
