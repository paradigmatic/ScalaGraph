package sg.gen

import sg.graphs._
import scala.collection.Iterable

trait GraphGenerator[V,E <: Edge[V]] {

  def apply[G <:Graph[V,E] with Modifiable[V,E]]( emptyGraph: G ): G

}

trait VertexGenerator[V] {

  def next(): V

}

object VertexGenerator {
  
  implicit def func2gen[V]( f: () => V ) = new VertexGenerator[V] {
    def next() = f()
  }

  implicit def iterator2gen[V]( iterator: Iterator[V] ) = 
    new VertexGenerator[V] {
      def next() = {
        if( iterator.hasNext ) {
          iterator.next()
        } else {
          throw new IllegalStateException("The iterator has not enough value.")
        }
      }
    }

  implicit def iterable2gen[V]( iterable: Iterable[V] ) = 
    iterator2gen( iterable.iterator )

}
