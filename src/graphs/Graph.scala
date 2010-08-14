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

  type G <: Modifiable[V]

  def add( vertex: V ): Boolean
  def connect( edge: E ): Boolean
  def remove( vertex: V): Boolean
  def disconnect( edge: E): Boolean
  def newGraph(): G
  //def copy(): G

  def add( vertices: Traversable[V] ): Boolean = 
    vertices.foldLeft(false)( _ | add(_) )

  def remove( vertices: Traversable[V] )(implicit m: Manifest[V]): Boolean = 
    vertices.foldLeft(false)( _ | remove(_) )

  def disconnect( edges: Traversable[E] )(implicit m:Manifest[E]): Boolean =
    edges.foldLeft(false)( _ |  disconnect(_) )
}

trait SimpleGraph[V] extends Graph[V] {
  type E <: SimpleEdge[V]
  trait SimpleEdge[V] extends Edge[V] {
    def first(): V
    def second(): V
  }
}
