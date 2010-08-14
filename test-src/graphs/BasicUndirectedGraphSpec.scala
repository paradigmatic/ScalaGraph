package sg.graphs

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class BasicUndirectedGraphSpec extends FlatSpec with ShouldMatchers {

  def emptyGraph = new BasicUndirectedGraph[Int]
  
  "A new basic undirected graph" should "be empty" in {
    val g = emptyGraph
    g.edges should be ('empty)
    g.vertices should be ('empty)
  }

  it should "support vertex addition" in {
    val g = emptyGraph
    (g contains 1) should be (false)
    g add 1
    (g contains 1) should be (true)
  }

  it should "return true iff the graph is modified when adding a vertex" in {
    val g = emptyGraph
    (g add 1) should be (true)
    (g add 1) should be (false)
    (g add 2) should be (true)
  }

  it can "connect vertices" in {
    val g = emptyGraph
    g add 1
    g add 2
    g add 3
    g.connect(1,2)
    ( g.connects(1,2) ) should be (true)
    ( g.connects(1,3) ) should be (false)
  }

  it should "add vertices that are not in the graph when creating edge" in {
    val g = emptyGraph
    g connect (1,2)
    (g contains 1) should be (true)
    (g contains 2) should be (true)
    (g connects (1,2)) should be (true)
  }

  it should "return true iff the graph is modified when adding an edge" in {
    val g = emptyGraph
    (g connect (1,2)) should be (true)
    (g connect (1,3)) should be (true)
    (g connect (1,2)) should be (false)
  }

  it should "not add several time the same edge" in {
    val g = emptyGraph
    g connect (1,2)
    (g connect (2,1)) should be (false)
    g.edges.size should be (1)
  }

  it should "have undirected edges" in {
    val g = emptyGraph
    g connect (1,2)
    (g connects (1,2)) should be (true)
    (g connects (2,1)) should be (true)
  }

  it can "find the neighborhood" in {
    val g = emptyGraph
    g connect (1,2)
    g connect (2,3)
    g add 4
    (g neighborsOf 1) should be (Set(2))
    (g neighborsOf 2) should be (Set(1,3))
    (g neighborsOf 3) should be (Set(2))
    (g neighborsOf 4) should be (Set())
  }

  it can "compute the degree" in {
    val g = emptyGraph
    g connect (1,2)
    g connect (2,3)
    g add 4
    (g degreeOf 1) should be (1)
    (g degreeOf 2) should be (2)
    (g degreeOf 3) should be (1)
    (g degreeOf 4) should be (0)
  }

  it must "throw an exception when asking for the neighborhood of a node not present" in {
    val g = emptyGraph
    intercept[NoSuchElementException] {
      g neighborsOf 1
    }
  }

 it must "throw an exception when asking for the degree of a node not present" in {
    val g = emptyGraph
    intercept[NoSuchElementException] {
      g degreeOf 1
    }
 }

  it can "remove edges" in {
    val g = emptyGraph
    g connect (1,2)
    g.connects(1,2) should be (true)
    val e = g.edges.toList.head
    g disconnect e
    g.connects(1,2) should be (false)
    g.edges should be ('empty)
  }

  it should "reply true iff edge exists when removing it" in {
    val g = emptyGraph
    g connect (1,2)
    val e = g.edges.toList.head
    (g disconnect e) should be (true)
    (g disconnect e) should be (false)
  }

  it can "remove vertex" in {
    val g = emptyGraph
    g add 1
    g add 2
    g.vertices.contains(1) should be (true)
    g remove 1
    g.vertices.contains(1) should be (false)
    g.vertices.contains(2) should be (true)
  }

  it should "reply true iff vertex exists when removing it" in {
    val g = emptyGraph
    g add 1
    g add 2
    (g remove 1) should be (true)
    (g remove 1) should be (false)
    (g remove 2) should be (true)
  }

  it should "remove incident edges when removing a vertex" in {
    val g = emptyGraph
    g connect (1,2)
    g.edges.size should be (1)
    g remove 1
    g.edges.size should be (0)
    (g contains 2) should be (true) 
  }

  it can "find the incident edges of a vertex" in {
    val g = emptyGraph
    g connect (1,2)
    g connect (2,3)
    g connect (2,4)
    g connect (1,4)
    g.incidentEdgesOf(1).size should be (2)
    g.incidentEdgesOf(2).size should be (3)
    g.incidentEdgesOf(3).size should be (1)
    g.incidentEdgesOf(4).size should be (2)
  }

  it can "add several vertices" in {
    val g = emptyGraph
    g add List(1,3,5)
    g.vertices should be (Set(1,3,5))
  }

  it should "return true if the graph is modified when adding several vertices" in {
    val g = emptyGraph
    g add 1
    (g add List(1,1,1,1)) should be (false)
    (g add List(1,1,1,2)) should be (true)
    g.vertices.size should be (2)
  }

  it can "remove several vertices" in {
    val g = emptyGraph
    g add List(1,3,5,7)
    g remove List(3,5)
    g.vertices should be (Set(1,7))
  }

   it should "return true if the graph is modified when removing several vertices" in {
     val g = emptyGraph
     g add List(1,3,5,7)
     g.remove(List(2,2,2)) should be (false)
     g.remove(List(1,1,1)) should be (true)
   }

  it can "remove several edges" in {
    val g = emptyGraph
    g connect (1,2)
    g connect (2,3)
    g connect (2,4)
    val (keep :: remove) = g.edges.filter( _ contains 1).toList
    g disconnect remove
    (g.edges contains keep) should be (true)
    (remove exists ( g.edges contains _ )) should be (false)
  }

  it can "produce an empty graph" in {
    val g = emptyGraph
    val g2 = g.newGraph
    g.getClass should be (g2.getClass)
  }

}
