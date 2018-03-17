package org.wouzar.hanoi

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object HanoiTowerSolution extends App {

  final case class Disk(radius: Int)

  final case class Pole(name: String,
                        var disks: scala.collection.mutable.Stack[Disk]) {
    def copy = {
      Pole(name, disks.filter(_ => true))
    }
  }

  final case class State(n: Int, p1: Pole, p2: Pole, p3: Pole) {
    def isSuccess: Boolean =
      p1.disks.size == 0 && p2.disks.size == n && p3.disks.size == 0

    def copy = {
      new State(n, this.p1.copy, this.p2.copy, this.p3.copy)
    }
  }

  def canMove(from: Pole, to: Pole) =
    from.disks.size > 0 &&
      (to.disks.size == 0 || to.disks.last.radius > from.disks.last.radius)

  def move(from: Pole, to: Pole) = {
    if (canMove(from, to)) to.disks.push(from.disks.pop())
    else
      throw new IllegalArgumentException(
        "There is no way move disk with bigger radius onto one with smaller!")
  }

  def show(p: Pole) = p.name + ": " + p.disks.mkString(",")

  def dfs = {

    val p1 = Pole("pole 1", mutable.Stack(Disk(1), Disk(2), Disk(3)))
    val p2 = Pole("pole 2", mutable.Stack())
    val p3 = Pole("pole 3", mutable.Stack())

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

  def bfs = {

    val p1 = Pole("pole 1", mutable.Stack(Disk(1), Disk(2)))
    val p2 = Pole("pole 2", mutable.Stack())
    val p3 = Pole("pole 3", mutable.Stack())

    val state = State(p1.disks.size, p1, p2, p3)
    var states = mutable.ArrayBuffer(state)

    def convert(state: State, from: Int, to: Int): (Pole, Pole) = {
      (if (from == 1) state.p1 else if (from == 2) state.p2 else state.p3,
       if (to == 1) state.p1 else if (to == 2) state.p2 else state.p3)
    }

    def children(initial: State, _from: Int, _to: Int) = {
      val (from, to) = convert(initial, _from, _to)
      if (canMove(from, to)) {
        println(_from + " -> " + _to)
        val newState = initial.copy

        {
          val (from, to) = convert(newState, _from, _to)

          val disk = from.disks.pop()
          to.disks.push(disk)
          states.append(newState)
        }
      }
    }

    while (states.nonEmpty) {
      val state = states(0)
      if (state.isSuccess) {
        println("Success")
        states = ArrayBuffer()
      } else {
        states = states.drop(1)

        children(state, 1, 2)
        children(state, 1, 3)
        children(state, 2, 2)
        children(state, 2, 1)
        children(state, 3, 2)
        children(state, 3, 1)

        println(s"States = ${states}")

      }
    }

  }

  def heuristic = {

    def _move(p1: Pole, p2: Pole) = {
      if (canMove(p1, p2)) move(p1, p2) else move(p2, p1)
    }

    val p1 = Pole("pole 1", mutable.Stack(Disk(1), Disk(2), Disk(3)))
    val p2 = Pole("pole 2", mutable.Stack())
    val p3 = Pole("pole 3", mutable.Stack())

    val n = p1.disks.size

    if (n % 2 == 0) {
      while (p3.disks.size != n) {
        _move(p1, p2)
        if (p3.disks.size != n) {
          _move(p1, p3)
        }
        if (p3.disks.size != n) {
          _move(p2, p3)
        }
      }
    } else {
      while (p3.disks.size != n) {
        _move(p1, p3)
        if (p3.disks.size != n) {
          _move(p1, p2)
        }
        if (p3.disks.size != n) {
          _move(p2, p3)
        }
      }

    }
    println("Success!")

  }

  heuristic

}
