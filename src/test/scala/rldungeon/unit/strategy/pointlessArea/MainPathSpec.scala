package rldungeon.unit.strategy.pointlessArea

import net.cyndeline.rlgraph.pathfinding.Path
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode}
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.help.{MainPath, PointlessArea}
import testHelpers.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class MainPathSpec extends SpecImports {
  private implicit def undiToCollapsed(e: UnDiEdge[CollapsedNode]): CollapsedEdge[CollapsedNode] = CollapsedEdge(e._1, e._2)

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

  private def pointlessAreasOnInnerNodes = new {
    val n = nodes
    import n._
    val mainEdge1 = CollapsedEdge(n1, n2)
    val mainEdge2 = CollapsedEdge(n2, n3)
    val mainEdge3 = CollapsedEdge(n3, n4)

    val pArea1Edge1 = CollapsedEdge(n2, n5)
    val pArea1Edge2 = CollapsedEdge(n5, n6)

    val pArea2Edge1 = CollapsedEdge(n3, n7)
    val pArea2Edge2 = CollapsedEdge(n7, n8)
    val pArea2Edge3 = CollapsedEdge(n7, n9)

    val totalGraph = Graph(mainEdge1, mainEdge2, mainEdge3, pArea1Edge1, pArea1Edge2, pArea2Edge1, pArea2Edge2, pArea2Edge3)
  }

  private def pointlessAreaAsPath = new {
    val n = nodes
    import n._
    val mainEdge1 = CollapsedEdge(n1, n2)
    val mainEdge2 = CollapsedEdge(n2, n3)
    val mainEdge3 = CollapsedEdge(n3, n4)

    // A pointless path [3]-5-6-7
    val PA1Edge1 = CollapsedEdge(n3, n5)
    val PA1Edge2 = CollapsedEdge(n5, n6)
    val PA1Edge3 = CollapsedEdge(n6, n7)

    // A pointless path [2]-8-9
    val PA2Edge1 = CollapsedEdge(n2, n8)
    val PA2Edge2 = CollapsedEdge(n8, n9)

    val totalGraph = Graph(mainEdge1, mainEdge2, mainEdge3, PA1Edge1, PA1Edge2, PA1Edge3, PA2Edge1, PA2Edge2)
  }

  describe("MainPath") {

    /*
     * Constructing paths
     */

    it ("should compute the initial main path") {

      Given("a graph with the main path 1 -> 4")
      val f = pointlessAreasOnInnerNodes
      import f._
      import f.n._

      When("constructing a main path using node 1 and 4 as start/goal")
      val mainPath = MainPath(totalGraph, n1, n4)

      Then("the resulting object should contain the path 1 -> 4")
      val expectedPath = Path(n1, Vector(mainEdge1, mainEdge2, mainEdge3))
      mainPath.currentPath should be (expectedPath)

    }

    it ("should compute the initial pointless areas of a graph") {

      Given("a graph with the main path 1 -> 4, with two pointless areas (5, 6) and (7,8,9) connected to 2 and 3")
      val f = pointlessAreasOnInnerNodes
      import f._
      import f.n._

      When("constructing a main path using node 1 and 4 as start/goal")
      val mainPath = MainPath(totalGraph, n1, n4)

      Then("two pointless areas should be found")
      mainPath.pointlessAreas should have size 2

      And("the pointless area 5,6 should be present")
      val subGraph1 = Graph(pArea1Edge1, pArea1Edge2)
      mainPath.pointlessAreas should contain (PointlessArea(subGraph1, n2))

      And("the pointless area 7,8,9 should be present")
      val subGraph2 = Graph(pArea2Edge1, pArea2Edge2, pArea2Edge3)
      mainPath.pointlessAreas should contain (PointlessArea(subGraph2, n3))

    }

    it ("should compute an empty set of pointless areas if the graph have none") {

      Given("a graph containing only the main path")
      val f = nodes
      import f._
      val graph = Graph[CollapsedNode, CollapsedEdge](n1~n2, n2~n3, n3~n4)

      When("constructing the main path")
      val mainPath = MainPath(graph, n1, n4)

      Then("no pointless areas should be found")
      mainPath.pointlessAreas should be ('empty)

    }

    it ("should construct a path that begins and ends in the same node") {

      Given("a graph with a single node 1 on the main path and a pointless area 1 -> 3 connected to it")
      val f = nodes
      import f._
      val graph = Graph[CollapsedNode, CollapsedEdge](n1~n2, n2~n3)

      When("constructing the main path using node 1 as both start and goal")
      val mainPath = MainPath(graph, n1, n1)

      Then("the main path shoud contain node 1")
      mainPath.currentPath should be (Path[CollapsedNode, CollapsedEdge](n1))

      And("nodes 2, 3 should be a pointless area")
      mainPath.pointlessAreas should have size 1
      mainPath.pointlessAreas.head should be (PointlessArea(graph, n1))

    }

    /*
     * Appending paths
     */

    it ("should append a single new node to a path containing a single node") {

      Given("a path with a single node 1 connected to a pointless area with a single node 2")
      val f = nodes
      import f._
      val edge = CollapsedEdge(n1, n2)
      val mainPath = MainPath(Graph[CollapsedNode, CollapsedEdge](edge), n1, n1)

      When("appending node 2 to the path")
      val appendedPath = mainPath.appendPath(n2, mainPath.pointlessAreas.head)

      Then("the resulting path should have no pointless areas")
      appendedPath.pointlessAreas should be ('empty)

      And("the main path should be 1 -> 2 -> 1")
      appendedPath.currentPath should be (Path(n1, Vector(edge, edge)))

    }

    it ("should append multiple nodes and edges to a path") {

      Given("a main path 1 -> 4 with the pointless area 7-8, 7-9 connected to node 3")
      val f = pointlessAreasOnInnerNodes
      import f._
      import f.n._
      val mainPath = MainPath(totalGraph, n1, n4)

      When("appending the pointless area using the target 8")
      val pa = findPointlessArea(mainPath, Graph(pArea2Edge1, pArea2Edge2, pArea2Edge3))
      val appendedPath = mainPath.appendPath(n8, pa)

      Then("the new main path should be 1, 2, 3, 7, 8, 7, 3, 4")
      val edges = Vector(mainEdge1, mainEdge2, pArea2Edge1, pArea2Edge2, pArea2Edge2, pArea2Edge1, mainEdge3)
      appendedPath.currentPath should be (Path(n1, edges))

      And("a new pointless area 7-9 should exist with main area connection 7")
      val newTopology = Graph(pArea2Edge3)
      appendedPath.pointlessAreas should have size 2
      appendedPath.pointlessAreas.exists(a => a.topology == newTopology)
      val area = findPointlessArea(appendedPath, newTopology)
      area.mainPathConnection should be (n7)

    }

    it ("should append a path at the end of the current path") {

      Given("a main path 1 -> 4 with a pointless area 4-5, 5-6")
      val f = pointlessAreasOnInnerNodes
      import f._
      import f.n._
      val e1 = CollapsedEdge(n4, n5)
      val e2 = CollapsedEdge(n5, n6)
      val graph = Graph(mainEdge1, mainEdge2, mainEdge3, e1, e2)
      val mainPath = MainPath(graph, n1, n4)

      When("appending the pointless area using the target 6")
      val pa = findPointlessArea(mainPath, Graph(e1, e2))
      val appendedPath = mainPath.appendPath(n6, pa)

      Then("the current path should be 1, 2, 3, 4, 5, 6, 5, 4")
      val expectedEdges = Vector(mainEdge1, mainEdge2, mainEdge3, e1, e2, e2, e1)
      appendedPath.currentPath should be (Path(n1, expectedEdges))

      And("no pointless areas should be found")
      appendedPath.pointlessAreas should be ('empty)

    }

    it ("should add splits from a previous pointless area to the back of the area vector") {

      Given("a graph with two pointless areas (one of them being 3-7, 7-8, 7-9)")
      val f = pointlessAreasOnInnerNodes
      import f._
      import f.n._
      val mainPath = MainPath(totalGraph, n1, n4)

      When("appending the path 3-7")
      val originalPA = findPointlessArea(mainPath, Graph(pArea2Edge1, pArea2Edge2, pArea2Edge3))
      val otherOriginalPA = findPointlessArea(mainPath, Graph(pArea1Edge1, pArea1Edge2))
      val appendedPath = mainPath.appendPath(n7, originalPA)


      Then("the two new pointless areas 7-8 and 7-9 should exist along the path")
      val pa1 = PointlessArea(Graph(pArea2Edge2), n7)
      val pa2 = PointlessArea(Graph(pArea2Edge3), n7)
      appendedPath.pointlessAreas should contain (pa1)
      appendedPath.pointlessAreas should contain (pa2)

      And("the two new areas should be at the tail of the list")
      appendedPath.pointlessAreas(0) should be (otherOriginalPA)
      Set(appendedPath.pointlessAreas(1), appendedPath.pointlessAreas(2)) should be (Set(pa1, pa2))

    }

    it ("should add a pointless area to the main path and remove it if the pointless area is a path") {

      Given("a graph with a pointless area 3-5-6-7 in the shape of a path")
      val f = pointlessAreaAsPath
      import f._
      import f.n._
      val mainPath = MainPath(totalGraph, n1, n4)
      val pa = findPointlessArea(mainPath, Graph(PA1Edge1, PA1Edge2, PA1Edge3))
      val otherPa = mainPath.pointlessAreas.find(_ != pa).get

      When("appending the path 3-7")
      val appendedPath = mainPath.appendPath(n7, pa)

      Then("only the other pointless area should remain")
      appendedPath.pointlessAreas should be (Vector(otherPa))

      And("the main path should be 1, 2, 3, 5, 6, 7, 6, 5, 3, 4")
      val edges = Vector(mainEdge1, mainEdge2, PA1Edge1, PA1Edge2, PA1Edge3, PA1Edge3, PA1Edge2, PA1Edge1, mainEdge3)
      appendedPath.currentPath should be (Path(n1, edges))

    }

    /*
     * Removing nodes
     */

    it ("should trim away nodes from pointless areas") {

      Given("a graph with two pointless areas (one of them being 3-7, 7-8, 7-9, 9-10)")
      val f = pointlessAreasOnInnerNodes
      import f._
      import f.n._
      val e = CollapsedEdge(n9, n10)
      val graph = totalGraph + e
      val mainPath = MainPath(graph, n1, n4)
      val pa = findPointlessArea(mainPath, Graph(pArea2Edge1, pArea2Edge2, pArea2Edge3, e))
      val otherPa = mainPath.pointlessAreas.find(_ != pa).get

      When("removing node 10")
      val resultingPath = mainPath.removeNode(n10, pa)

      Then("the two areas should remain")
      resultingPath.pointlessAreas should have size 2

      And("the head area should have the topology 3-7, 7-8")
      val expectedTopology = Graph(pArea2Edge1, pArea2Edge2)
      resultingPath.pointlessAreas should be (Vector(PointlessArea(expectedTopology, n3), otherPa))

    }

    it ("should remove an entire pointless area if it is a path") {

      Given("a graph with a pointless area 3-5-6-7 in the shape of a path")
      val f = pointlessAreaAsPath
      import f._
      import f.n._
      val mainPath = MainPath(totalGraph, n1, n4)
      val pa = findPointlessArea(mainPath, Graph(PA1Edge1, PA1Edge2, PA1Edge3))
      val otherPa = mainPath.pointlessAreas.find(_ != pa).get

      When("removing node 7")
      val resultingPath = mainPath.removeNode(n7, pa)

      Then("the entire area should be removed")
      resultingPath.pointlessAreas should be (Vector(otherPa))

    }

  }

  private def findPointlessArea(mainPath: MainPath, topology: Graph[CollapsedNode, CollapsedEdge]): PointlessArea = {
    mainPath.pointlessAreas.find(_.topology == topology).getOrElse(throw new Error("Setup error: Could not find pointless area."))
  }
}
