package sg.graphs

import scala.collection.Set

trait Graph[V] {
  type E <: Edge[V]
  def vertices: Set[V]
  def edges: Set[E]
  def contains( vertex: V ) = vertices contains vertex
  def connects( v1: V, v2: V ) = edges exists ( _ connects (v1, v2) )
  trait Edge[V] {
    def connects( v1: V, v2: V ): Boolean
    def vertices(): Set[V]
  }
}

trait Modifiable[V] { 
  self: Graph[V] =>
  def add( vertex: V ): Boolean
  def add( edge: E ): Boolean
}

trait SimpleGraph[V] extends Graph[V] {
  type E <: SimpleEdge[V]
  trait SimpleEdge[V] extends Edge[V] {
    def first(): V
    def second(): V
  }
}
