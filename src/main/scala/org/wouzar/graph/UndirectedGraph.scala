package org.wouzar.graph

import java.util

import org.wouzar.graph.UndirectedGraph.{Edge, Vertex}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object UndirectedGraph {

  final case class Vertex(value: Int) extends AnyVal
  final case class Edge(source: Vertex, target: Vertex) // todo : change for digraph

}

// todo : add Readme and pictures for algo
trait Graph {

  def edges: Seq[Edge]

  def edgesOf(v: Vertex): Seq[Edge] = edges.filter(e => e.source == v || e.target == v)

}

case class UndirectedGraph(edges: Seq[Edge]) extends Graph {

  val vertices: Seq[Vertex] = edges.flatMap(x => Seq(x.source, x.target))

  def getEdgeSource(e: Edge): Vertex = e.source

  def getEdgeTarget(e: Edge): Vertex = e.target

  def cycleBase(): List[List[Vertex]] = {
    if (this == null) throw new IllegalArgumentException("Null graph.")
    val used = new mutable.HashMap[Vertex, mutable.HashSet[Vertex]]()
    val parent = new util.HashMap[Vertex, Vertex]()
    val stack = new util.ArrayDeque[Vertex]()
    val cycles = new ArrayBuffer[ArrayBuffer[Vertex]]
    for (root <- this.vertices) { // Loop over the connected
      // components of the graph.
      if (parent.containsKey(root)) {} else {
        // Free some memory in case of
        // multiple connected components.
        used.clear
        // Prepare to walk the spanning tree.
        parent.put(root, root)
        used.put(root, new mutable.HashSet[Vertex]())
        stack.push(root)
        // Do the walk. It is a BFS with
        // a LIFO instead of the usual
        // FIFO. Thus it is easier to
        // find the cycles in the tree.
        while (!stack.isEmpty) {
          val current = stack.pop
          val currentUsed = used(current)
          import scala.collection.JavaConversions._
          for (e <- this.edgesOf(current)) {
            var neighbor = this.getEdgeTarget(e)
            if (neighbor.equals(current)) neighbor = this.getEdgeSource(e)
            if (!used.containsKey(neighbor)) { // found a new node
              parent.put(neighbor, current)
              val neighbourUsed = new mutable.HashSet[Vertex]()
              neighbourUsed.add(current)
              used.put(neighbor, neighbourUsed)
              stack.push(neighbor)
            }
            else if (neighbor.equals(current)) { // found a self loop
              val cycle = new ArrayBuffer[Vertex]
              cycle.add(current)
              cycles.add(cycle)
            }
            else if (!currentUsed.contains(neighbor)) { // found a cycle
              val neighbourUsed = used(neighbor)
              val cycle = new ArrayBuffer[Vertex]()
              cycle.add(neighbor)
              cycle.add(current)
              var p = parent(current)
              while (!neighbourUsed.contains(p)) {
                cycle.add(p)
                p = parent(p)
              }
              cycle.add(p)
              cycles.add(cycle)
              neighbourUsed.add(current)
            }
          }
        }
      }
    }
    cycles.map(_.toList).toList
  }

  def circumference: Int = {

    def xor(set: Set[Edge], that: Set[Edge]): Set[Edge] =
      set.union(that) diff set.intersect(that)

    // without ordering of vertices
    val longestCycle = cycleBase().foldLeft(Set[Edge]()) { case (set, cycle) =>
      val edges = (cycle.last :: cycle) zip cycle

      xor(set, edges.map { case (x, y) =>
        val first = if (x.value > y.value) x else y
        val second = if (x.value == first.value) y else x
        Edge(first, second)
      }.toSet)
    }

    longestCycle.size

  }

}
