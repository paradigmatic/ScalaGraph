package sg.gen

import sg.graphs._
import scala.util.Random

class ErdosRenyiGenerator[V]( val vGen: VertexGenerator[V],
                                            val rng: Random,
                                            val numVertex: Int,
                                            val p: Double
                                         ) 
extends UndirectedGraphGenerator[V] {

  def apply[G <:UndirectedGraph[V] with Modifiable[V,_]]
  ( graph: G ) = {
    /*val seq = (0 until numVertex).toSeq map ( i => vGen.next )
    graph add seq
    for( i <- 0 until numVertex; j <- i+1 until numVertex ) {
      if( rng.nextDouble <= p ) {
        graph connect ( seq(i), seq(j) )
      }
    }
    */
    for( i <- 0 until numVertex ) {
      val v = vGen.next
      for( w <- graph.vertices ) {
        if( rng.nextDouble <= p ) {
          graph connect (v, w)
        }
      }
      graph add v
    }
    graph
  }

}

object  ErdosRenyiGenerator {

  def apply[V]( numVertex: Int, p: Double, rng: Random )
  ( vGen: VertexGenerator[V] ) = 
    new ErdosRenyiGenerator( vGen, rng, numVertex, p )
  
    def apply[V]( numVertex: Int, p: Double )
  ( vGen: VertexGenerator[V] ) = 
    new ErdosRenyiGenerator( vGen, new Random, numVertex, p )

}
