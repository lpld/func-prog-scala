package com.github.lpld.study.funcscala.chapter6

/**
  * Random numbers generator
  */
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  /*
   * 6.1
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /*
   * 6.2
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    val ii = if (i > 0) i - 1 else -(i + 1)
    (ii.toDouble / Int.MaxValue, r)
  }

  /*
   * 6.3
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  /*
   * 6.4
   */
  def ints(rng: RNG)(count: Int): (List[Int], RNG) =
    if (count == 0) (List(), rng)
    else {
      val (l, r) = ints(rng)(count - 1)
      val (i, r2) = r.nextInt
      (i :: l, r2)
    }

  def chooseInt(rNG: RNG)(start: Int, stopExclusive: Int): (Int, RNG) = {
    val (i, r) = rNG.nextInt
    val diff = stopExclusive - start
    (start + i % diff, r)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  val nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  /*
   * 6.5
   */
  val doubleViaMap: Rand[Double] = map(_.nextInt)(i => (if (i > 0) i - 1 else -(i + 1)).toDouble / Int.MaxValue)

  /*
   * 6.6
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def intDoubleViaBoth: Rand[(Int, Double)] = both(_.nextInt, double)

  /*
   * 6.7
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => unit(Nil)
    case a :: as => map2(a, sequence(as))(_ :: _)
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))(map2(_, _)(_ :: _))

  /*
   * 6.8
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (v, rng2) = f(rng)
    g(v)(rng2)
  }

  /*
   * 6.9
   */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(f andThen unit)

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(f.curried(a)))


  //  def mapViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  //    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
}


case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
