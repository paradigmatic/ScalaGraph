package sg.graphs

import scala.collection.Set

trait UndirectedGraph[V] extends SimpleGraph[V] {
  type E = UndirectedEdge[V]
  class UndirectedEdge[V](val first: V, val second: V ) extends SimpleEdge[V] {
    val vertices = Set( first, second )
    def connects(v1: V, v2: V) = 
      (vertices contains v1 ) && (vertices contains v2 ) 
  }
}

class BasicUndirectedGraph[V] extends UndirectedGraph[V] with Modifiable[V]{
  private[this] var edgeSet = Set[E]()

  def add( vertex: V ) = false
  def add( edge: E ) = { edgeSet += edge; false  }
  def addEdge( v1: V, v2: V ) = add( new UndirectedEdge(v1,v2) )

  def vertices = Set[V]()
  def edges = edgeSet
}
