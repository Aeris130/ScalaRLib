package rldungeon.unit.strategy

import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help._
import rldungeon.help.RoomVertex
import testHelpers.SpecImports

import scala.language.implicitConversions
import scalax.collection.GraphEdge.{DiEdge, UnDiEdge}
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class SuperNodeFactorySpec extends SpecImports {
  private implicit def edge2CollapsedEdgeAssoc[A <: CollapsedNode](e: UnDiEdge[A]): CollapsedEdgeAssoc[A] = new CollapsedEdgeAssoc(e)
  private implicit def edge2DiCollapsedEdgeAssoc[A <: CollapsedNode](e: DiEdge[A]): DiCollapsedEdgeAssoc[A] = new DiCollapsedEdgeAssoc(e)
  private val factory = new SuperNodeFactory()

  def vertices = new {
    val n1 = new RoomVertex(1)
    val n2 = new RoomVertex(2)
    val n3 = new RoomVertex(3)
    val n4 = new RoomVertex(4)
    val n5 = new RoomVertex(5)
    val n6 = new RoomVertex(6)
    val n7 = new RoomVertex(7)
    val n8 = new RoomVertex(8)
    val n9 = new RoomVertex(9)
    val n10 = new RoomVertex(10)

    val cn1 = new CollapsedNode(Set(n1.rid))
    val cn2 = new CollapsedNode(Set(n2.rid))
    val cn3 = new CollapsedNode(Set(n3.rid))
    val cn4 = new CollapsedNode(Set(n4.rid))
    val cn5 = new CollapsedNode(Set(n5.rid))
    val cn6 = new CollapsedNode(Set(n6.rid))
    val cn7 = new CollapsedNode(Set(n7.rid))
    val cn8 = new CollapsedNode(Set(n8.rid))
    val cn9 = new CollapsedNode(Set(n9.rid))
    val cn10 = new CollapsedNode(Set(n10.rid))
  }

  describe("SuperNodeFactory") {

    it ("should collapse a single cycle") {

      Given("a graph with a single cycle")
      val f = vertices
      import f._
      val g = Graph[RoomVertex, UnDiEdge](n1~n2, n2~n3, n3~n1)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)

      Then("the resulting graph should contain a single super-node with all three vertices")
      val sn = new CollapsedNode(Set(n1.rid, n2.rid, n3.rid))
      collapsedGraph should equal (Graph[CollapsedNode, CollapsedEdge](sn))

    }

    it ("should collapsed a single vertex") {

      Given("a graph with a single vertex")
      val f = vertices
      import f._
      val g = Graph[RoomVertex, UnDiEdge](n1)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)

      Then("the resulting graph should contain a single node with the vertex")
      val sn = new CollapsedNode(n1.rid)
      collapsedGraph should equal (Graph[CollapsedNode, CollapsedEdge](sn))

    }

    it ("should collapse an empty graph") {

      Given("an empty graph")
      val f = vertices
      val g = Graph[RoomVertex, UnDiEdge]()

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)

      Then("the resulting graph should be empty")
      collapsedGraph should be ('empty)

    }

    it ("should join two cycles sharing a single vertex with a dummy edge") {

      Given("a graph with two cycles sharing a vertex")
      val f = vertices
      import f._
      val g = Graph[RoomVertex, UnDiEdge](n1~n2, n2~n3, n3~n1, n3~n4, n4~n5, n5~n3)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)

      Then("the resulting graph should contain two super-nodes with the common vertex, and two dummy edges joining them")
      val node1 = new CollapsedNode(Set(n1.rid, n2.rid, n3.rid))
      val node2 = new CollapsedNode(Set(n4.rid, n5.rid, n3.rid))
      val dummy3 = new CollapsedNode(Set(n3.rid)).asDummy
      val dummyEdge1 = CollapsedEdge.dummyEdge(node1, dummy3)
      val dummyEdge2 = CollapsedEdge.dummyEdge(node2, dummy3)
      collapsedGraph should equal (Graph(dummyEdge1, dummyEdge2))

    }

    it ("should collapse two cycles sharing multiple vertices") {

      Given("a graph with two cycles sharing two vertices and having one unique vertex each")
      val f = vertices
      import f._
      val g = Graph[RoomVertex, UnDiEdge](n1~n2, n2~n3, n3~n1, n3~n4, n4~n2)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)

      Then("the resulting graph should contain a single super-node with all four vertices")
      val superNode = new CollapsedNode(Set(n1.rid, n2.rid, n3.rid, n4.rid))
      collapsedGraph should equal (Graph[CollapsedNode, CollapsedEdge](superNode))

    }

    it ("should not collapse vertices that aren't a part of a cycle") {

      Given("a graph with a single cycle and two vertices connected to it")
      val f = vertices
      import f._
      val g = Graph[RoomVertex, UnDiEdge](n1~n2, n2~n3, n3~n1, n1~n4, n3~n5)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)

      Then("the resulting graph should still contain vertices n4 and n5")
      assert(collapsedGraph.nodes.exists(_ == cn4), cn4 + " was not found in the graph " + collapsedGraph)
      assert(collapsedGraph.nodes.exists(_ == cn5), cn5 + " was not found in the graph " + collapsedGraph)

    }

    it ("should reconnect vertices adjacent to newly created super-nodes with new edges") {

      Given("a graph with a single cycle and two vertices connected to it")
      val f = vertices
      import f._
      val g = Graph[RoomVertex, UnDiEdge](n1~n2, n2~n3, n3~n1, n1~n4, n3~n5)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)
      val superNode = new CollapsedNode(Set(n1.rid, n2.rid, n3.rid))

      Then("there should be an edge from n4 to the super-node (n1, n2, n3)")
      assert(edgeExists(collapsedGraph, superNode, cn4), "No edge was found between " + superNode + " and " + cn4)

      And("there should be an edge from n5 to the super-node (n1, n2, n3)")
      assert(edgeExists(collapsedGraph, superNode, cn5), "No edge was found between " + superNode + " and " + cn5)

    }

    it ("should store the original vertex an edge was connected to when the vertex is merged into a super-node") {

      Given("a graph with a single cycle and two vertices connected to it")
      val f = vertices
      import f._

      /* Note that the edges are: n4 <-> n1 and n3 <-> n5. The first edge has the non-cycle vertex in position _1, while
       * the other edge is in _2. This is to check that both positions are added correctly.
       */
      val g = Graph[RoomVertex, UnDiEdge](n1~n2, n2~n3, n3~n1, n4~n1, n3~n5)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)
      val superNode = new CollapsedNode(Set(n1.rid, n2.rid, n3.rid))

      Then("the edge from n4 to the super-node should have n1 stored as the original vertex as in the 'originalTo position")
      val edgeN4ToSuper = collapsedGraph.edges.find(e => e._1 == cn4).getOrElse(
        fail("No edge with " + cn4 + " in the first position")
      )

      edgeN4ToSuper._2 should equal (superNode)
      edgeN4ToSuper.originalFrom should be (None)
      edgeN4ToSuper.originalTo should equal (Option(n1.rid))

      Then("the edge from the super-node to n5 should have n3 stored as the original vertex as in the 'originalFrom position")
      val edgeSuperToN5 = collapsedGraph.edges.find(e => e._2 == cn5).getOrElse(
        fail("No edge with " + cn5 + " in the first position")
      )

      edgeSuperToN5._1 should equal (superNode)
      edgeSuperToN5.originalFrom should equal (Option(n3.rid))
      edgeSuperToN5.originalTo should be (None)

    }

    it ("should collapse multiple separate cycles") {

      Given("a graph with two separate cycles")
      val f = vertices
      import f._
      val g = Graph[RoomVertex, UnDiEdge](n1~n2, n2~n3, n3~n1, n4~n5, n5~n6, n6~n4)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)
      val superNode1 = new CollapsedNode(Set(n1.rid, n2.rid, n3.rid))
      val superNode2 = new CollapsedNode(Set(n4.rid, n5.rid, n6.rid))

      Then("two super-nodes should be in the graph")
      collapsedGraph should equal (Graph[CollapsedNode, CollapsedEdge](superNode1, superNode2))

    }

    it ("should add edges between super-nodes") {

      Given("a graph with two separate cycles connected by an edge between n3 and n4")
      val f = vertices
      import f._
      val g = Graph[RoomVertex, UnDiEdge](n1~n2, n2~n3, n3~n1, n4~n5, n5~n6, n6~n4, n3~n4)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)
      val superNode1 = new CollapsedNode(Set(n1.rid, n2.rid, n3.rid))
      val superNode2 = new CollapsedNode(Set(n4.rid, n5.rid, n6.rid))

      Then("there should be an edge between the two super-nodes")
      val edge = collapsedGraph.edges.find(e => e._1 == superNode1 && e._2 == superNode2).getOrElse(
        fail("No edge between " + superNode1 + " and " + superNode2 + " in the graph " + collapsedGraph)
      )

      And("the edge should have the vertex n3 in the originalFrom storage")
      edge.originalFrom should equal (Option(n3.rid))

      And("the edge should have the vertex n4 in the originalTo storage")
      edge.originalTo should equal (Option(n4.rid))

    }

    it ("should leave edges between vertices not a part of a cycle as-is") {

      Given("a graph with a cycle and two vertices (n4, n5) outside it with an edge between them")
      val f = vertices
      import f._
      val g = Graph[RoomVertex, UnDiEdge](n1~n2, n2~n3, n3~n1, n3~n4, n4~n5)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)

      Then("there should be an edge between n4 and n5")
      val edge = collapsedGraph.edges.find(e => e._1 == cn4 && e._2 == cn5).getOrElse(
        fail("No edge between " + cn4 + " and " + cn5 + " in the graph " + collapsedGraph)
      )

      And("the edge should have no vertices stored as original targets")
      edge.originalFrom should be (None)
      edge.originalTo should be (None)

    }

    it ("should collapse a cycle of cycles") {

      Given("a cycle n1->n2->n3->n4->n1, with additional cycles sharing one vertex (n1, n2, and with n3->n4 as a part of another 3-vertex cycle")
      val f = vertices
      import f._
      val mainCycle = Graph[RoomVertex, UnDiEdge](n1~n2, n2~n3, n3~n4, n4~n1)
      val extraCycle1 = Graph[RoomVertex, UnDiEdge](n1~n5, n5~n6, n6~n1)
      val extraCycle2 = Graph[RoomVertex, UnDiEdge](n2~n7, n7~n8, n8~n2)
      val extraCycle3 = Graph[RoomVertex, UnDiEdge](n3~n4, n4~n9, n9~n3)
      val g = mainCycle union extraCycle1 union extraCycle2 union extraCycle3

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)

      Then("a super-node containing all vertices 1, 2, 3, 4, 9 should be created")
      val mainNode = new CollapsedNode(Set(n1.rid, n2.rid, n3.rid, n4.rid, n9.rid))

      And("the super-node should contain a dummy edge to a single-node containing vertex 1, and another with vertex 2")
      val dummy1 = new CollapsedNode(Set(n1.rid)).asDummy
      val dummy2 = new CollapsedNode(Set(n2.rid)).asDummy
      val dummyEdge1 = CollapsedEdge.dummyEdge(dummy1, mainNode)
      val dummyEdge2 = CollapsedEdge.dummyEdge(dummy2, mainNode)

      And("the single vertex 1 should contain a dummy edge to a super-node with vertices 1, 5, 6")
      val superN1 = new CollapsedNode(Set(n1.rid, n5.rid, n6.rid))
      val dummyEdge3 = CollapsedEdge.dummyEdge(dummy1, superN1)

      And("the single vertex 2 should contain a dummy edge to a super-node with vertices 2, 7, 8")
      val superN2 = new CollapsedNode(Set(n2.rid, n7.rid, n8.rid))
      val dummyEdge4 = CollapsedEdge.dummyEdge(dummy2, superN2)

      collapsedGraph should equal (Graph(dummyEdge1, dummyEdge2, dummyEdge3, dummyEdge4))

    }

    it ("should maintain original targets when reconnecting a cycle-free vertex multiple times") {

      Given("a graph with two cycles sharing a vertex and a vertex connected to one of them")
      val f = vertices
      import f._
      val g = Graph[RoomVertex, UnDiEdge](n1~n2, n2~n3, n3~n1, n3~n4, n4~n5, n5~n3, n6~n3)

      When("collapsing all cycles, causing the edge from n6 to be reconnected to a dummy")
      val collapsedGraph = factory.collapseCycles(g)
      val dummy = new CollapsedNode(Set(n3.rid)).asDummy

      Then("the edge n6 -> dummy should not have original targets")
      val superNode1 = new CollapsedNode(Set(n1.rid, n2.rid, n3.rid))
      val superNode2 = new CollapsedNode(Set(n4.rid, n5.rid, n3.rid))
      val edgeToDummy = CollapsedEdge(cn6, dummy)
      val sn1ToDummy = CollapsedEdge.dummyEdge(dummy, superNode1)
      val sn2ToDummy = CollapsedEdge.dummyEdge(dummy, superNode2)

      collapsedGraph should equal (Graph(edgeToDummy, sn1ToDummy, sn2ToDummy))
    }

    it ("should assign each vertex its own collapsed node in a graph with no cycles") {

      Given("a graph with no cycles")
      val f = vertices
      import f._
      val g = Graph[RoomVertex, UnDiEdge](n1~n2, n2~n3, n2~n4)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)

      Then("a graph with the same topology should be created")
      collapsedGraph should equal (Graph[CollapsedNode, CollapsedEdge](CollapsedEdge(cn1, cn2), CollapsedEdge(cn2, cn3), CollapsedEdge(cn2, cn4)))

    }

    it ("should return an empty graph when collapsing an empty graph") {

      Given("an empty graph")
      val g = Graph[RoomVertex, UnDiEdge]()

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)

      Then("the resulting graph should be empty")
      collapsedGraph should equal (Graph[CollapsedNode, CollapsedEdge]())

    }

    it ("should preserve the positions of vertices in the original edges between singular rooms") {

      Given("a graph with two vertices 1 and 2 and an edge between them with vertex 1 in the first position and vertex 2 in the second position")
      val f = vertices
      import f._
      val g = Graph[RoomVertex, UnDiEdge](n1~n2)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(g)
      val edge = collapsedGraph.edges.head.toOuter // The only edge

      Then("the resulting edge should have the super-node representing vertex 1 in the first position")
      edge._1.singleRepresentedVertex should be (n1.rid)

      And("the resulting edge should have the super-node representing vertex 2 in the second position")
      edge._2.singleRepresentedVertex should be (n2.rid)

    }

    it ("should preserve the positions of vertices in the original edge between cycles") {

      Given("a graph with two cycles connected by an edge with a vertex from cycle 1 in the first position and cycle 2 in the second position")
      val f = vertices
      import f._
      val c1 = Graph(n1~n2, n2~n3, n3~n1)
      val c2 = Graph(n4~n5, n5~n6, n6~n4)
      val e = n3~n4
      val graph = (c1 union c2) + e

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(graph)
      val edge = collapsedGraph.edges.head.toOuter // The only edge

      Then("the node representing cycle 1 should be in the first position")
      edge._1.vertexCollection should be (Set(n1.rid, n2.rid, n3.rid))
      edge.originalFrom should be (Option(n3.rid))

      And("the node representing cycle 2 should be in the second position")
      edge._2.vertexCollection should be (Set(n4.rid, n5.rid, n6.rid))
      edge.originalTo should be (Option(n4.rid))

    }

    it ("should preserve the positions of vertices between original edges between a single vertex and a cycle") {

      Given("a graph with an edge between a single room in the first position and a cycle in the second position")
      val f = vertices
      import f._
      val graph = Graph(n1~n2, n2~n3, n3~n1) + n4~n3

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(graph)
      val edge = collapsedGraph.edges.head.toOuter // The only edge

      Then("the node representing the single vertex should be in the first position")
      edge._1.singleRepresentedVertex should be (n4.rid)

      And("the node representing the cycle should be in the second position")
      edge._2.vertexCollection should be (Set(n1.rid, n2.rid, n3.rid))
      edge.originalTo should be (Option(n3.rid))

    }

    it ("should collapsed a graph with no cycles into a separate super-node for each vertex") {

      Given("a tree")
      val f = vertices
      import f._
      val graph = Graph(n1~n2, n2~n3, n2~n4)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(graph)

      Then("there should be an edge from n1 to n2")
      assert(collapsedGraph.edges.exists(e => e._1.singleRepresentedVertex == n1.rid && e._2.singleRepresentedVertex == n2.rid), "No edge from " + n1 + " to " + n2 + " found in " + collapsedGraph)

      And("there should be an edge from n2 to n3")
      assert(collapsedGraph.edges.exists(e => e._1.singleRepresentedVertex == n2.rid && e._2.singleRepresentedVertex == n3.rid), "No edge from " + n2 + " to " + n3 + " found in " + collapsedGraph)

      And("there should be an edge from n2 to n4")
      assert(collapsedGraph.edges.exists(e => e._1.singleRepresentedVertex == n2.rid && e._2.singleRepresentedVertex == n4.rid), "No edge from " + n2 + " to " + n4 + " found in " + collapsedGraph)

    }

    it ("should collapse cycles into a single super-node") {

      Given("three cycles sharing vertices")
      val f = vertices
      import f._
      val graph = Graph(n1~n2, n2~n3, n3~n1, n1~n4, n4~n2, n4~n3)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(graph)

      Then("a single super-node with vertices 1, 2, 3, 4, 5 should be present")
      collapsedGraph.size should be (1)
      collapsedGraph.nodes.head.vertexCollection should be (Set(n1.rid, n2.rid, n3.rid, n4.rid))

    }

    it ("should connect biconnected components with multiple edges sharing a cutpoint with a dummy edge") {

      Given("three biconnected components sharing a single vertex (3)")
      val f = vertices
      import f._
      val c1 = Graph(n1~n2, n2~n3, n3~n1)
      val c2 = Graph(n4~n5, n5~n3, n3~n4)
      val c3 = Graph(n6~n7, n7~n3, n3~n6)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(c1 ++ c2 ++ c3)

      Then("there should be a collapsed node representing only vertex 1")
      val cn3 = collapsedGraph.nodes.find(_.vertexCollection == Set(3)).getOrElse(fail("No super-node representing the cutpoint."))

      And("that node should have three edges connected to it")
      cn3.edges should have size 3

      And("every such edge should be a dummy")
      cn3.edges.count(_.isDummy) should be (3)

    }

    it ("should preserve directed edges") {

      Given("a graph with two nodes connected by a directed edge")
      val f = vertices
      import f._
      val graph = Graph(n1~>n2)

      When("collapsing all cycles")
      val collapsedGraph = factory.collapseCycles(graph)

      Then("the two collapsed nodes should be connected by a directed collapsed edge")
      val cn1 = new CollapsedNode(1)
      val cn2 = new CollapsedNode(2)
      val edge: DiCollapsedEdge[CollapsedNode] = (cn1~>cn2).emptyEdge()
      val expectedGraph = Graph[CollapsedNode, DiCollapsedEdge](edge)
      collapsedGraph should be (expectedGraph)

    }

    ignore ("should preserve directed edges in both directions") {

      Given("a graph with two nodes connected by two directed edges")
      val f = vertices
      import f._
      val graph = Graph(n1~>n2, n2~>n1)

      When("collapsing all cycles")
      val collapsedGraph: Graph[CollapsedNode, CollapsedEdge] = factory.collapseCycles(graph)

      Then("the two collapsed nodes should be connected by a directed collapsed edge")
      val cn1 = new CollapsedNode(1)
      val cn2 = new CollapsedNode(2)
      val edge1 = (cn1~>cn2).emptyEdge()
      val edge2 = (cn2~>cn1).emptyEdge()
      val expectedGraph = Graph[CollapsedNode, CollapsedEdge](edge1, edge2)
      collapsedGraph should be (expectedGraph)
    }

  }

  private def edgeExists(g: Graph[CollapsedNode, CollapsedEdge], a: CollapsedNode, b: CollapsedNode): Boolean = {
    g.edges.toOuter.exists(e => (e._1 == a && e._2 == b) || (e._2 == a && e._1 == b))
  }
}
