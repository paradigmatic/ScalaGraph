package sg.features

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import sg.graphs._

class ConnectedComponentsSpec extends FlatSpec with ShouldMatchers {
  
  "The connected components object" can "split a graph in connected components" in {
    val g = new BasicUndirectedGraph[Int]()
    g connect (1,2)
    g connect (2,3)
    g connect (2,7)
    g connect (4,5)
    g add (6)
    val comps = ConnectedComponents.of( g )
    comps.size should be (3)
    val sizes = comps map ( _.vertices.size )
    sizes should contain (4)
    sizes should contain (2)
    sizes should contain (1)
    val all = comps.foldLeft( Set[Int]() )( _ ++ _.vertices )
    all should be (g.vertices)
  }

  it should "return an empty list if the graph is empty" in {
    val g = new BasicUndirectedGraph[Int]()
    val comps = ConnectedComponents.of( g )
    comps should have size (0)
  }

}
