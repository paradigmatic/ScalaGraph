package sg.features

import scala.collection.Set
import sg.graphs._

object ConnectedComponents {

  def of[V,E <: Edge[V]]( g: Graph[V,E] with Modifiable[V,E] ) = {
    var comps = List[Graph[V,E] with Modifiable[V,E]]()
    var visited = Set[V]()
    var flagOut = true
    while( visited != g.vertices && flagOut) {
      println("Iteration -- visited: " + visited )
      val start = ( g.vertices -- visited ).toList.head
      println("start: "+start)
      var searchLst = List(start)
      var compVertices = Set[V]()
      while( ! searchLst.isEmpty ) {
        val (next :: rest) = searchLst
        println("  next: " + next)
        println("  rest: " + rest)
        searchLst = rest
        compVertices += next
        println("  compVertices: " + compVertices)
        for( v <- g neighborsOf next if !(compVertices contains v ) ){
          searchLst ::= v
        }
      }
      val comp = g.newGraph
      for( v <- compVertices; e <- g incidentEdgesOf v ) {
        comp connect e
      }
      comp add compVertices
      comps ::= comp
      println( "comps: "+ comps)
      visited ++= compVertices
      
      flagOut = true
    }
    comps
  }

}
