package sg.graph

import scala.collection.Set

trait Graph[V] {

  type E <: Edge[V]

  def vertices: Set[V]
  def edges: Set[E]
  def contains( vertex: V ) = vertices contains vertex
  def connects( v1: V, v2: V ) = edges exists ( e => {
    val vs = e.vertices
    ( vs contains v1 ) && ( vs contains v2 )
  })

  trait Edge[V] {
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
    def vertexPair(): (V,V)
    def vertices = Set( vertexPair._1, vertexPair._2 )
  }
}

trait UndirectedGraph[V] extends SimpleGraph[V] {
  type E = UndirectedEdge[V]
  class UndirectedEdge[V](v1: V, v2: V ) extends SimpleEdge[V] {
    def vertexPair() = (v1,v2)
  }
}

class BasicUndirectedGraph[V] extends UndirectedGraph[V] with Modifiable[V]{
  private[this] var edgeSet = Set[E]()

  def add( vertex: V ) = false
  def add( edge: E ) = { edgeSet += edge; false  }
  def connect( v1: V, v2: V ) = add( new UndirectedEdge(v1,v2) )

  def vertices = Set[V]()
  def edges = edgeSet
}
