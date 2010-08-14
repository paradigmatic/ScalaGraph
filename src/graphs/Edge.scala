package sg.graphs 

trait Edge[V] {
  def connects( v1: V, v2: V ): Boolean
  def vertices(): Set[V]
  def contains( vertex: V ) = vertices contains vertex
}


trait SimpleEdge[V] extends Edge[V] {
  def first(): V
  def second(): V
}

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
