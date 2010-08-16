package sg.gen

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import sg.graphs._

class ErdosRenyiGeneratorSpec extends FlatSpec with ShouldMatchers {
  
  "An Erdos-Renyi generator" should "return the empty graph if the number of asked vertices is 0" in {
    val g = new BasicUndirectedGraph[Int]
    val gen = ErdosRenyiGenerator( 0, 0.5 )( () => 2 )
    gen(g).vertices should have size (0)
  }

  it should "return unconnected vertices if the prob is 0" in {
    val g = new BasicUndirectedGraph[Int]
    var i = 0
    val gen = ErdosRenyiGenerator( 100, 0.0 )( () => {i+=1; i} )
    gen(g)
    g.vertices should have size (100)
    g.edges should be ('empty)
  }
  
  it should "return a full connected graph if the prob is 1" in {
    val g = new BasicUndirectedGraph[Int]
    var i = 0
    val gen = ErdosRenyiGenerator( 100, 1.0 )( () => {i+=1; i} )
    gen(g)
    g.vertices should have size (100)
    g.edges should have size (100*99/2)
    for( v <- g.vertices; w <- g.vertices ) {
      g.connects(v,w) should be (true)
    }
  }


}
