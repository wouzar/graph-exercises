package org.wouzar.hanoi

import scala.collection.mutable

object HanoiTowerSolution extends App {

  final case class Disk(radius: Int)
  final case class Pole(name: String, var disks: scala.collection.mutable.Stack[Disk])

  def move(from: Pole, to: Pole) = {
    to.disks.push(from.disks.pop())
  }

  def dfs = {

    val p1 = Pole("pole 1", mutable.Stack(Disk(3), Disk(2), Disk(1)))
    val p2 = Pole("pole 2", mutable.Stack())
    val p3 = Pole("pole 3", mutable.Stack())

    def show(p: Pole) = p.name + ": " + p.disks.mkString(",")

    def rec(n: Int, src: Pole, dst: Pole, aux: Pole): Unit = {
      if (n > 0) {
        rec(n - 1, src, aux, dst)
        println(s"Move: ${src.name} -> ${dst.name}")
        move(src, dst)
        println(show(p1))
        println(show(p2))
        println(show(p3))
        println("------")
        rec(n - 1, aux, dst, src)
      }
    }

    rec(3, p1, p2, p3)

  }

   dfs

}
