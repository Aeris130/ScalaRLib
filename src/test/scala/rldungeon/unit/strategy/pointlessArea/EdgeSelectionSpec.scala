package rldungeon.unit.strategy.pointlessArea

import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{DiCollapsedEdge, CollapsedEdge, CollapsedNode}
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.help.EdgeSelection
import testHelpers.SpecImports

class EdgeSelectionSpec extends SpecImports {
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
    val initialAreaConnections = vertices.toSet

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

    val edges = Vector(e1, e2, e3, e4, e5, e6, e7, e7, e6, e5, e4, e10, e11, e11, e10, e8, e9)
    val initialAreaConnections = Set(n1, n2, n3, n4)
  }

  // Main: 1 to 4, loops 2-5-2 and 3-6-3
  private def mainPathWithTwoLoops = new {
    val n = nodes
    import n._

    val vertices = Vector(n1, n2, n5, n2, n3, n6, n3, n4)

    val e1 = CollapsedEdge(n1, n2)
    val e2 = CollapsedEdge(n2, n3)
    val e3 = CollapsedEdge(n3, n4)
    val e4 = CollapsedEdge(n2, n5)
    val e5 = CollapsedEdge(n3, n6)

    val edges = Vector(e1, e4, e4, e2, e5, e5, e3, e4)
    val initialAreaConnections = Set(n1, n2, n3, n4)
  }

  describe("EdgeSelection") {

    it ("should return edges after the first node on the main path if it is the area connection") {

      Given("a main path 1-2-3")
      val f = nodes
      import f._
      val e1 = CollapsedEdge(n1, n2)
      val e2 = CollapsedEdge(n2, n3)
      val initialAreaConnections = Set(n1, n2, n3)

      When("computing the edge candidates using node 1 as the main connection")
      val candidates = selection.findEdgeCandidates(Vector(n1, n2, n3), Vector(e1, e2), n1, initialAreaConnections).map(_.edge)

      Then("edge 1-2 and 2-3 should be returned")
      candidates should be (Vector(e1, e2))

    }

    it("should return every edge after the main connection when the main path doesn't loop") {

      Given("a path 1 -> 7 with no loops")
      val f = mainPathWithoutLoop
      import f._
      import f.n._

      When("computing the edge candidates using node 4 as the main connection")
      val candidates = selection.findEdgeCandidates(vertices, edges, n4, initialAreaConnections).map(_.edge)

      Then("four edges should be found")
      candidates should have size 4

      And("the edge candidates should be 4-5, 5-6, 6-7, 7-8")
      candidates should be (Vector(e4, e5, e6, e7))

    }

    it ("should return multiple edge sections if the main path loops") {

      Given("a main path that loops from 4 to 8 then back, then 4, 11, 12, 11, 4, then 9, 10")
      val f = mainPathWithLoop
      import f._
      import f.n._

      When("computing the edge candidates using node 4 as the main connection")
      val candidates = selection.findEdgeCandidates(vertices, edges, n4, initialAreaConnections).map(_.edge)

      Then("eight edges should be found")
      candidates should have size 8

      And("the three foremost edges should be 4-5, 4-9 and 4-11 in any order")
      Set(candidates(0), candidates(1), candidates(2)) should be (Set(e4, e8, e10))

      And("the three next candidates should be 5-6, 9-10 and 11-12 in any order")
      Set(candidates(3), candidates(4), candidates(5)) should be (Set(e5, e9, e11))

      And("the remaining candidates should be 6-7, 7-8 in that order")
      candidates(6) should be (e6)
      candidates(7) should be (e7)

    }

    it ("should not return dummy edges") {

      Given("a main path with three nodes and two dummy edges connecting them")
      val f = nodes
      import f._
      val e1 = CollapsedEdge.dummyEdge(n1, n2)
      val e2 = CollapsedEdge.dummyEdge(n2, n3)

      When("computing the edge candidates using node 1 as the main connection")
      val candidates = selection.findEdgeCandidates(Vector(n1, n2, n3), Vector(e1, e2), n1, Set(n1, n2, n3)).map(_.edge)

      Then("no result should be found")
      candidates should be ('empty)

    }

    it ("should not return edge candidates that lie beyond a single directed edge") {

      Given("a main path 1-2->3-4")
      val f = nodes
      import f._
      val e1 = CollapsedEdge(n1, n2)
      val e2 = DiCollapsedEdge(n2, n3)
      val e3 = CollapsedEdge(n3, n4)

      When("computing the edge candidates using node 1 as the main connection")
      val candidates = selection.findEdgeCandidates(Vector(n1, n2, n3, n4), Vector(e1, e2, e3), n1, Set(n1, n2, n3)).map(_.edge)

      Then("edges 1-2 and 2->3 should be returned as candidates")
      candidates should have size 2
      candidates(0) should be (e1)
      candidates(1) should be (e2)

    }

    it ("should not return duplicate candidates") {

      Given("a graph where the edges 2-5 and 3-6 are visited twice")
      val f = mainPathWithTwoLoops
      import f._
      import f.n._

      When("computing the edge candidates using the first node as the main connection")
      val candidates = selection.findEdgeCandidates(vertices, edges, n1, initialAreaConnections).map(_.edge)

      Then("every edge along the main path should be returned once")
      candidates should have size 5
      candidates.toSet should be (Set(e1, e2, e3, e4, e5))

    }

    it ("should not return any candidates if no edges exists") {

      Given("a main path with a single node")
      val f = nodes
      import f._

      When("computing the edge candidates using that node as the main connection")
      val candidates = selection.findEdgeCandidates(Vector(n1), Vector(), n1, Set(n1)).map(_.edge)

      Then("No candidates should be returned")
      candidates should be ('empty)

    }

    it ("should return the starting node of an edge candidate") {

      Given("a main path 1-2-5-2-3-6-3-4")
      val f = mainPathWithTwoLoops
      import f._
      import f.n._

      When("computing candidates using node 5 as main connection")
      val candidates = selection.findEdgeCandidates(vertices, edges, n5, initialAreaConnections)

      Then("the edge 2-3 should be selected using node 2 as start")
      val e23c = candidates.find(_.edge == e2).get
      e23c.from should be (n2)

      And("the edge 3-6 should be selected using node 3 as start")
      val e36c = candidates.find(_.edge == e5).get
      e36c.from should be (n3)

      And("the edge 3-4 should be selected using node 3 as start")
      val e34c = candidates.find(_.edge == e3).get
      e34c.from should be (n3)

    }

    it ("should not return candidates if the area connection is the goal node") {

      Given("a main path ending in node 8")
      val f = mainPathWithoutLoop
      import f._
      import f.n._

      When("computing candidates using node 8 as main connection")
      val candidates = selection.findEdgeCandidates(vertices, edges, n8, initialAreaConnections)

      Then("no candidates should be returned")
      candidates should be ('empty)

    }

  }
}

