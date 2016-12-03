package com.github.lpld.study.funcscala.chapter6

/**
  * @author leopold
  * @since 30/11/16
  */
sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def input(i: Input): Machine = i match {
    case Coin => Machine(
      locked = candies <= 0,
      candies = candies,
      coins + 1)
    case Turn => Machine(
      locked = true,
      candies = if (locked) candies else candies - 1,
      coins = coins
    )
  }
}

object Machine {
  val machine = new Machine(locked = true, candies = 10, coins = 5)

  def work(i: Input): State[Machine, (Int, Int)] = State((m: Machine) => {
    val newM = m.input(i)
    ((newM.candies, newM.coins), newM)
  })

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State.sequence(inputs.map(work)).map(_.last)
  }
}