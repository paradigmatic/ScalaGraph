package sg.gen

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class VertexGeneratorSpec extends FlatSpec with ShouldMatchers {
  
  import VertexGenerator._

  "A vertex generator" can "be instanciated from a function" in {
    val f = () => 4
    val g:VertexGenerator[Int] = f
    g.next should be (4)
    g.next should be (4)
  }

  it can "be instanciated from an iterator" in {
    val l = List(2,4)
    val g:VertexGenerator[Int] = l.iterator
    g.next should be (2)
    g.next should be (4)
  }

  it should "throw an IllegalStateException if not enough values are present" in {
    val l = List(1)
    val g:VertexGenerator[Int] = l.iterator
    intercept[IllegalStateException] {
      g.next
      g.next
    }
  }

  it can "be instanciated from any iterable collection" in {
   val l = List(2,4)
    val g:VertexGenerator[Int] = l
    g.next should be (2)
    g.next should be (4)
  }

}
