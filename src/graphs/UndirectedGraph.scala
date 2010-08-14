package sg.graphs

import scala.collection.Set

trait UndirectedGraph[V] extends SimpleGraph[V] {

  type E = UndirectedEdge[V]

  class UndirectedEdge[V](val first: V, val second: V ) extends SimpleEdge[V] {

    val vertices = Set( first, second )

    def connects(v1: V, v2: V) = 
      (vertices contains v1 ) && (vertices contains v2 ) 

    override def equals(other: Any) = other match {
      case that : UndirectedEdge[_] => {
        that.isInstanceOf[UndirectedEdge[_]] &&
        this.vertices == that.vertices
      }
      case _ => false
    }

    override def hashCode() = vertices.hashCode
  }
}

class BasicUndirectedGraph[V] extends UndirectedGraph[V] with Modifiable[V]{
  var edges = Set[E]()
  var vertices = Set[V]()

  def add( vertex: V ) = if( vertices contains vertex ) {
    false
  } else {
    vertices += vertex
    true
  }

  def add( edge: E ) = if( edges contains edge ) {
    false
  } else {
    add( edge.first )
    add( edge.second )
    edges += edge
    true
  }

  def addEdge( v1: V, v2: V ) = add( new UndirectedEdge(v1,v2) )

}
