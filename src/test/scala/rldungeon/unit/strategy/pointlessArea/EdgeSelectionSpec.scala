package rldungeon.unit.strategy.pointlessArea

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode}
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.help.EdgeSelection

@RunWith(classOf[JUnitRunner])
class EdgeSelectionSpec extends FunSpec with GivenWhenThen with ShouldMatchers {
  private val selection = new EdgeSelection()

  private def nodes = new {
    val n1 = new CollapsedNode(1)
    val n2 = new CollapsedNode(2)
    val n3 = new CollapsedNode(3)
    val n4 = new CollapsedNode(4)
    val n5 = new CollapsedNode(5)
    val n6 = new CollapsedNode(6)
    val n7 = new CollapsedNode(7)
    val n8 = new CollapsedNode(8)
    val n9 = new CollapsedNode(9)
    val n10 = new CollapsedNode(10)
    val n11 = new CollapsedNode(11)
    val n12 = new CollapsedNode(12)
    val n13 = new CollapsedNode(13)
    val n14 = new CollapsedNode(14)
  }

  private def mainPathWithoutLoop = new {
    val n = nodes
    import n._

    val vertices = Vector(n1, n2, n3, n4, n5, n6, n7, n8)

    val e1 = CollapsedEdge(n1, n2)
    val e2 = CollapsedEdge(n2, n3)
    val e3 = CollapsedEdge(n3, n4)
    val e4 = CollapsedEdge(n4, n5)
    val e5 = CollapsedEdge(n5, n6)
    val e6 = CollapsedEdge(n6, n7)
    val e7 = CollapsedEdge(n7, n8)

    val edges = Vector(e1, e2, e3, e4, e5, e6, e7)

  }

  // Goes from 1 to 4, loops 5, 6, 7, 8, 7, 6, 5, 4 then ends 9, 10
  private def mainPathWithLoop = new {
    val n = nodes
    import n._

    val vertices = Vector(n1, n2, n3, n4, n5, n6, n7, n8, n7, n6, n5, n4, n11, n12, n11, n4, n9, n10)

    val e1 = CollapsedEdge(n1, n2)
    val e2 = CollapsedEdge(n2, n3)
    val e3 = CollapsedEdge(n3, n4)
    val e4 = CollapsedEdge(n4, n5)
    val e5 = CollapsedEdge(n5, n6)
    val e6 = CollapsedEdge(n6, n7)
    val e7 = CollapsedEdge(n7, n8)
    val e8 = CollapsedEdge(n4, n9)
    val e9 = CollapsedEdge(n9, n10)
    val e10 = CollapsedEdge(n4, n11)
    val e11 = CollapsedEdge(n11, n12)

    val edges = Vector(e1, e2, e3, e4, e5, e6, e7, e7,  e6, e5, e4, e10, e11, e11, e10, e8, e9)
  }

  describe("EdgeSelection") {

    it("should return every edge after the main connection when the main path doesn't loop") {

      Given("a path 1 -> 7 with no loops")
      val f = mainPathWithoutLoop
      import f._
      import f.n._

      When("computing the edge candidates using node 4 as the main connection")
      val candidates = selection.findEdgeCandidates(vertices, edges, n4)

      Then("a single section of edges should be found")
      candidates should have size 1

      And("the edge candidates should be 4-5, 5-6, 6-7, 7-8")
      candidates.head should be (Vector(e4, e5, e6, e7))

    }

    it ("should return multiple edge sections if the main path loops") {

      Given("a main path that loops from 4 to 8 then back, then 4, 11, 12, 11, 4")
      val f = mainPathWithLoop
      import f._
      import f.n._

      When("computing the edge candidates using node 4 as the main connection")
      val candidates = selection.findEdgeCandidates(vertices, edges, n4)

      Then("two edge sections should be found")
      candidates should have size 2

      And("one of the candidates should be edges 4-5-6-7-8")
      assert(candidates.exists(_ == Vector(e4, e5, e6, e7)))

      And("one of the candidates should be edges 4-11-12")
      assert(candidates.exists(_ == Vector(e10, e11)))

    }

    it ("should not return dummy edges") {

      Given("a main path with three nodes and two dummy edges connecting them")
      val f = nodes
      import f._
      val e1 = CollapsedEdge.dummyEdge(n1, n2)
      val e2 = CollapsedEdge.dummyEdge(n2, n3)

      When("computing the edge candidates using node 1 as the main connection")
      val candidates = selection.findEdgeCandidates(Vector(n1, n2, n3), Vector(e1, e2), n1)

      Then("no result should be found")
      candidates should be ('empty)

    }

  }
}

