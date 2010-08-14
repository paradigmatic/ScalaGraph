package sg.graphs

import scala.collection.Set

trait Graph[V] {
  type E <: Edge[V]

  def vertices: Set[V]

  def edges: Set[E]

  def contains( vertex: V ) = vertices contains vertex

  def connects( v1: V, v2: V ) = edges exists ( _ connects (v1, v2) )

  def incidentEdgesOf( vertex: V ):Set[E] = 
    edges filter (_ contains vertex)

  def neighborsOf( vertex: V ) = {
    if( ! contains(vertex) ) {
      throw new NoSuchElementException("The vertex: "
                                       +vertex+ " is not member of the graph")
    }
    var hood = Set[V]()
    for( e <- incidentEdgesOf(vertex) ) {
      hood ++= ( e.vertices - vertex )
    }
    hood
  }

  def degreeOf( vertex: V ) = neighborsOf(vertex).size

  trait Edge[V] {
    def connects( v1: V, v2: V ): Boolean
    def vertices(): Set[V]
    def contains( vertex: V ) = vertices contains vertex
  }

}

trait Modifiable[V] { 
  self: Graph[V] =>
  def add( vertex: V ): Boolean
  def add( vertices: Traversable[V] ): Boolean = 
    vertices.foldLeft(false)( _ | add(_) )
  def add( edge: E ): Boolean
  def remove( vertex: V): Boolean
  def remove( vertices: Traversable[V] ): Boolean = 
    vertices.foldLeft(false)( _ | remove(_) ) 
  def remove( edge: E): Boolean
}

trait SimpleGraph[V] extends Graph[V] {
  type E <: SimpleEdge[V]
  trait SimpleEdge[V] extends Edge[V] {
    def first(): V
    def second(): V
  }
}
