package sg.graphs

import scala.collection.Set

trait UndirectedGraph[V] extends SimpleGraph[V,UndirectedEdge[V]] {
  type E = UndirectedEdge[V]
}

class BasicUndirectedGraph[V] extends UndirectedGraph[V] with Modifiable[V,UndirectedGraph[V]#E]{
  type G = BasicUndirectedGraph[V]

  var edges = Set[E]()
  var vertices = Set[V]()

  def newGraph() = new BasicUndirectedGraph[V]()
  
  def copy() = {
    val g = new BasicUndirectedGraph[V]()
    for( e <- edges ) { g connect e }
    g
  }

  def add( vertex: V ) = if( vertices contains vertex ) {
    false
  } else {
    vertices += vertex
    true
  }

  def connect( edge: E ): Boolean = if( edges contains edge ) {
    false
  } else {
    add( edge.first )
    add( edge.second )
    edges += edge
    true
  }

  def connect( v1: V, v2: V ): Boolean = connect( new UndirectedEdge(v1,v2) )

  def remove( vertex: V) = if( vertices contains vertex ) {
    vertices -= vertex
    for( e <- incidentEdgesOf(vertex) ) {
      disconnect(e)
    }
    true
  } else false
  
  def disconnect( edge: E) = if( edges contains edge ) {
    edges -= edge
    true 
  } else false
}
