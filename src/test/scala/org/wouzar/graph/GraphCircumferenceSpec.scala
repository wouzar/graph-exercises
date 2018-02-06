package org.wouzar.graph

import org.scalatest._
import org.wouzar.graph.UndirectedGraph.{Edge, Vertex}

class GraphCircumferenceSpec extends FlatSpec {

  implicit class CyclicSeq[T](seq: Seq[T]) {

    def equivalents(that: Seq[T]): Boolean = {
      if (seq.size != that.size) false else {
        val variations = for {
          n <- 1 to seq.size
        } yield {
          val (a, b) = seq.splitAt(n)
          b ++ a
        }

        variations.contains(that)
      }
    }

  }

  "Presented graph" should "have correct cycle base" in {
    val base = new UndirectedGraph(
      Seq(
        Edge(Vertex(1), Vertex(2)),
        Edge(Vertex(1), Vertex(4)),
        Edge(Vertex(2), Vertex(3)),
        Edge(Vertex(2), Vertex(4)),
        Edge(Vertex(3), Vertex(4))
      ))

    val result = base.cycleBase()
    assert(List(1, 2, 4).map(Vertex) equivalents result.head)
    assert(List(2, 3, 4).map(Vertex) equivalents result.tail.head)

  }


  "Presented graph" should "have cycle base size equal 3" in {
    val graph = new UndirectedGraph(
      Seq(
        Edge(Vertex(1), Vertex(2)),
        Edge(Vertex(1), Vertex(6)),
        Edge(Vertex(2), Vertex(3)),
        Edge(Vertex(2), Vertex(6)),
        Edge(Vertex(3), Vertex(4)),
        Edge(Vertex(3), Vertex(5)),
        Edge(Vertex(4), Vertex(5)),
        Edge(Vertex(5), Vertex(6))
      ))

    assert(graph.cycleBase.size == 3)
  }

  "Presented graph" should "have circumference equal 6" in {
    val graph = new UndirectedGraph(
      Seq(
        Edge(Vertex(1), Vertex(2)),
        Edge(Vertex(1), Vertex(6)),
        Edge(Vertex(2), Vertex(3)),
        Edge(Vertex(2), Vertex(6)),
        Edge(Vertex(3), Vertex(4)),
        Edge(Vertex(3), Vertex(5)),
        Edge(Vertex(4), Vertex(5)),
        Edge(Vertex(5), Vertex(6))
      ))

    assert(graph.circumference == 6)
  }

}