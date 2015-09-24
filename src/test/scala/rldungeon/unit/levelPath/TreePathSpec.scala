package rldungeon.unit.levelPath

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import rldungeon.help.{CorridorEdgeAssoc, GraphLevel, CorridorEdge, RoomVertex}
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedNode, CollapsedEdge, SuperNodeFactory}
import net.cyndeline.scalarlib.rlgraph.pathfinding.{BFSPathfinder, Path}
import net.cyndeline.scalarlib.rldungeon.levelPath.{Branch, TreeNode, TreePath}

@RunWith(classOf[JUnitRunner])
class TreePathSpec extends FunSpec with GivenWhenThen with ShouldMatchers {
  private implicit def edge2Assoc(e: UnDiEdge[RoomVertex]) = new CorridorEdgeAssoc(e)
  private val superNFactory = new SuperNodeFactory()
  private val pathfinder = new BFSPathfinder()

  private def rooms = new {
    val r1 = new RoomVertex(1)
    val r2 = new RoomVertex(2)
    val r3 = new RoomVertex(3)
    val r4 = new RoomVertex(4)
    val r5 = new RoomVertex(5)
    val r6 = new RoomVertex(6)
    val r7 = new RoomVertex(7)
    val r8 = new RoomVertex(8)
  }

  private def mainPathNoLoops = new {
    val rms = rooms
    import rms._
    val e_1_2 = r1~r2 cid 1
    val e_2_3 = r2~r3 cid 2
    val e_3_4 = r3~r4 cid 3
    val e_4_5 = r4~r5 cid 4
    val graph = Graph(e_1_2, e_2_3, e_3_4, e_4_5)
    val level = GraphLevel(graph)
    val collapsed = superNFactory.collapseCycles(graph)
    val mainPath = pathfinder.computePath(findNode(collapsed, r1), findNode(collapsed, r5), collapsed).get
  }

  private def mainPathWithLoop = new {
    val rms = rooms
    import rms._
    val e_1_2 = r1~r2 cid 1
    val e_2_3 = r2~r3 cid 2
    val e_3_4 = r3~r4 cid 3
    val e_2_5 = r2~r5 cid 4
    val graph = Graph(e_1_2, e_2_3, e_3_4, e_2_5)
    val level = GraphLevel(graph)
    val collapsed = superNFactory.collapseCycles(graph)
    val ce1 = findEdge(collapsed, r1, r2)
    val ce2 = findEdge(collapsed, r2, r3)
    val ce3 = findEdge(collapsed, r3, r4)
    val ce4 = findEdge(collapsed, r2, r5)

    // Loops 1-2-3-4-3-2-5
    val mainPath = Path(findNode(collapsed, r1), Vector(ce1, ce2, ce3, ce3, ce2, ce4))
  }

  private def multieEdgeComponents = new {
    val rms = rooms
    import rms._
    val c1 = Graph(r1~r2 cid 1, r2~r3 cid 2, r3~r1 cid 3)
    val c2 = Graph(r3~r4 cid 4, r4~r5 cid 5, r5~r3 cid 6)
    val graph = c1 ++ c2
    val level = GraphLevel(graph)
    val collapsed = superNFactory.collapseCycles(graph)
    val mainPath = pathfinder.computePath(findNode(collapsed, r1), findNode(collapsed, r5), collapsed).get
  }

  private def twoPointlessAreasSameConnection = new {
    val rms = rooms
    import rms._
    val edge1 = r2~r4 cid 3
    val edge2 = r2~r5 cid 4
    val area1 = Graph(edge1)
    val area2 = Graph(edge2)
    val graph = Graph(r1~r2 cid 1, r2~r3 cid 2) ++ area1 ++ area2
    val level = GraphLevel(graph)
    val collapsed = superNFactory.collapseCycles(graph)
    val mainPath = pathfinder.computePath(findNode(collapsed, r1), findNode(collapsed, r3), collapsed).get
  }

  private def twoPointlessAreasDifferentConnection = new {
    val rms = rooms
    import rms._
    val edge1 = r2~r5 cid 4
    val edge2 = r3~r6 cid 5
    val area1 = Graph(edge1)
    val area2 = Graph(edge2)
    val graph = Graph(r1~r2 cid 1, r2~r3 cid 2, r3~r4 cid 3) ++ area1 ++ area2
    val level = GraphLevel(graph)
    val collapsed = superNFactory.collapseCycles(graph)
    val mainPath = pathfinder.computePath(findNode(collapsed, r1), findNode(collapsed, r4), collapsed).get
  }

  private def forkingPointlessArea = new {
    val rms = rooms
    import rms._
    val edge1 = r2~r4 cid 4
    val edge2 = r4~r5 cid 5
    val edge3 = r4~r6 cid 6
    val area = Graph(edge1, edge2, edge3)
    val graph = Graph(r1~r2 cid 1, r2~r3 cid 2) ++ area
    val level = GraphLevel(graph)
    val collapsed = superNFactory.collapseCycles(graph)
    val mainPath = pathfinder.computePath(findNode(collapsed, r1), findNode(collapsed, r3), collapsed).get
  }

  describe("TreePath") {

    /*
     * Main path
     */

    it ("should direct a main path without loops through the level") {

      Given("a collapsed main path from 1 to 5")
      val f = mainPathNoLoops
      import f._
      import f.rms._

      When("constructing a tree path")
      val path = TreePath(level, collapsed, mainPath)

      Then("the main path should contain 1 tree node for each edge and biconnected component)")
      path.mainPath.vertices should have size 9
      val n1 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r1))
      val n2 = TreeNode.corridorNode[RoomVertex, CorridorEdge](e_1_2)
      val n3 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r2))
      val n4 = TreeNode.corridorNode[RoomVertex, CorridorEdge](e_2_3)
      val n5 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r3))
      val n6 = TreeNode.corridorNode[RoomVertex, CorridorEdge](e_3_4)
      val n7 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r4))
      val n8 = TreeNode.corridorNode[RoomVertex, CorridorEdge](e_4_5)
      val n9 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r5))
      val expectedNodes = Vector(n1, n2, n3, n4, n5, n6, n7, n8, n9)
      path.mainPath.vertices should be (expectedNodes)

      And("each node should be connected by an edge specifying the node pairs connection")
      path.mainPath.edges should have size 8
      val expectedEdges = Vector(Branch(n1, n2, r1.rid), Branch(n2, n3, r2.rid), Branch(n3, n4, r2.rid),
                                  Branch(n4, n5, r3.rid), Branch(n5, n6, r3.rid), Branch(n6, n7, r4.rid),
                                  Branch(n7, n8, r4.rid), Branch(n8, n9, r5.rid))
      path.mainPath.edges should be (expectedEdges)

    }

    it ("should direct a main path with loops through the level") {

      Given("a collapsed main path 1 -> (loop start) 2 -> 3 -> 4 -> 2 (loop end) -> 5")
      val f = mainPathWithLoop
      import f._
      import f.rms._

      When("constructing a tree path")
      val path = TreePath(level, collapsed, mainPath)

      Then("the main path should contain double entries for room2 and single entries for the rest")
      val n1 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r1))
      val n2 = TreeNode.corridorNode[RoomVertex, CorridorEdge](e_1_2)
      val n3 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r2))
      val n4 = TreeNode.corridorNode[RoomVertex, CorridorEdge](e_2_3)
      val n5 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r3))
      val n6 = TreeNode.corridorNode[RoomVertex, CorridorEdge](e_3_4)
      val n7 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r4))
      val n8 = TreeNode.corridorNode[RoomVertex, CorridorEdge](e_2_5)
      val n9 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r5))
      val expectedNodes = Vector(n1, n2, n3, n4, n5, n6, n7, n6, n5, n4, n3, n8, n9)
      path.mainPath.vertices should be (expectedNodes)

      And("the loop should direct edges in the same direction as it is traversed")
      val expectedEdges = Vector(Branch(n1, n2, r1.rid), Branch(n2, n3, r2.rid), Branch(n3, n4, r2.rid),
        Branch(n4, n5, r3.rid), Branch(n5, n6, r3.rid), Branch(n6, n7, r4.rid),
        Branch(n7, n6, r4.rid), Branch(n6, n5, r3.rid), Branch(n5, n4, r3.rid),
        Branch(n4, n3, r2.rid), Branch(n3, n8, r2.rid), Branch(n8, n9, r5.rid))
      path.mainPath.edges should be (expectedEdges)

    }

    it ("should fold edges of a biconnected component with multiple edges into the same node") {

      Given("a collapsed main path with two components 1-2-3 and 3-4-5")
      val f = multieEdgeComponents
      import f._
      import f.rms._

      When("constructing a tree path")
      val path = TreePath(level, collapsed, mainPath)

      Then("the main path should contain two tree nodes with their entire biconnected components, joined by a node representing the cutpoint")
      val n1 = TreeNode.roomNode(c1)
      val n2 = TreeNode.cutPoint[RoomVertex](r3)
      val n3 = TreeNode.roomNode(c2)
      path.mainPath.vertices should be (Vector(n1, n2, n3))

      And("the nodes should be connected by directed edges in the direction of the path")
      path.mainPath.edges should be (Vector(Branch(n1, n2, r3.rid), Branch(n2, n3, r3.rid)))

    }

    it ("should create a main path when the entire level contains a single biconnected component") {

      Given("a level with a single room")
      val f = rooms
      import f._
      val graph = Graph[RoomVertex, CorridorEdge](r1)
      val level = GraphLevel(graph)
      val collapsed = superNFactory.collapseCycles(graph)
      val mainPath = Path[CollapsedNode, CollapsedEdge](collapsed.nodes.head)

      When("constructing a tree path")
      val path = TreePath(level, collapsed, mainPath)

      Then("the level should contain a single tree nodes representing room 1")
      val expectedNode = TreeNode.roomNode(graph)
      path.mainPath should be (Path[TreeNode, Branch](expectedNode))

    }

    /*
     * Pointless areas
     */

    it ("should detect two pointless areas sharing a connection point") {

      Given("a main path from room 1 to 3, with two pointless areas connected to room 2 (room 4 and 5)")
      val f = twoPointlessAreasSameConnection
      import f._
       import f.rms._

      When("constructing a tree path")
      val path = TreePath(level, collapsed, mainPath)

      Then("the path should contain a pointless area directed from room 2 to 4")
      val n2 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r2))
      val n2To4 = TreeNode.corridorNode[RoomVertex, CorridorEdge](edge1)
      val n4 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r4))
      val branch1 = Branch(n2, n2To4, r2.rid)
      val branch2 = Branch(n2To4, n4, r4.rid)

      path.pointlessAreas should contain (Graph(branch1, branch2))

      And("the path should contain a pointless area directed from room 2 to 5")
      val n2To5 = TreeNode.corridorNode[RoomVertex, CorridorEdge](edge2)
      val n5 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r5))
      val branch2_1 = Branch(n2, n2To5, r2.rid)
      val branch2_2 = Branch(n2To5, n5, r5.rid)

      path.pointlessAreas should contain (Graph(branch2_1, branch2_2))

    }

    it ("should detect two pointless areas with different connection points") {

      Given("a main path from room 1 to 4, with two pointless areas (from 2 to 5 and 3 to 6)")
      val f = twoPointlessAreasDifferentConnection
      import f._
      import f.rms._

      When("constructing a tree path")
      val path = TreePath(level, collapsed, mainPath)

      Then("the path should contain a pointless area directed from room 2 to 5")
      val n2 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r2))
      val n2To5 = TreeNode.corridorNode[RoomVertex, CorridorEdge](edge1)
      val n5 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r5))
      val branch1 = Branch(n2, n2To5, r2.rid)
      val branch2 = Branch(n2To5, n5, r5.rid)

      path.pointlessAreas should contain (Graph(branch1, branch2))

      And("the path should contain a pointless area directed from room 3 to 6")
      val n3 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r3))
      val n3To6 = TreeNode.corridorNode[RoomVertex, CorridorEdge](edge2)
      val n6 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r6))
      val branch2_1 = Branch(n3, n3To6, r3.rid)
      val branch2_2 = Branch(n3To6, n6, r6.rid)

      path.pointlessAreas should contain (Graph(branch2_1, branch2_2))

    }

    it ("should compute pointless areas with multiple edges that fork") {

      Given("a main path 1-3 with a pointless area connected to 2, with an edge to 4 that branches to 5 and 6")
      val f = forkingPointlessArea
      import f._
      import f.rms._

      When("constructing a tree path")
      val path = TreePath(level, collapsed, mainPath)

      Then("a single pointless area should exist with nodes 2->4, 4->5 and 4->6")
      path.pointlessAreas should have size 1
      val area = path.pointlessAreas.head
      area.nodes should have size 7

      val n2 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r2))
      val n4 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r4))
      val n5 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r5))
      val n6 = TreeNode.roomNode(Graph[RoomVertex, CorridorEdge](r6))

      val en1 = TreeNode.corridorNode[RoomVertex, CorridorEdge](edge1) // 2->4
      val en2 = TreeNode.corridorNode[RoomVertex, CorridorEdge](edge2) // 4->5
      val en3 = TreeNode.corridorNode[RoomVertex, CorridorEdge](edge3) // 4->6

      val branch1 = Branch(n2, en1, r2.rid)
      val branch2 = Branch(en1, n4, r4.rid)
      val branch3 = Branch(n4, en2, r4.rid)
      val branch4 = Branch(n4, en3, r4.rid)
      val branch5 = Branch(en2, n5, r5.rid)
      val branch6 = Branch(en3, n6, r6.rid)

      area should equal (Graph(branch1, branch2, branch3, branch4, branch5, branch6))

    }

    /*
     * Edge ID's
     */

    it ("should add the correct edge id's to nodes representing single edges") {

      Given("a main path with an edge 2->3 having id 2, and an edge 3->6 in a pointless area having id 5")
      val f = twoPointlessAreasDifferentConnection
      import f._
      import f.rms._

      When("constructing a tree path")
      val path = TreePath(level, collapsed, mainPath)

      Then("the node representing the edge 2->3 should have id 2")
      val edgeNode = path.mainPath.vertices(3) // 0 = 1, 1 == 1->2, 2 = 2, 3 = 2->3
      edgeNode.corridor should be (2)

      And("the node representing edge 3->6 in the pointless area should have id 5")
      val area = path.pointlessAreas.find(a => a.nodes.exists(n => n.isRoom && n.roomStructure.contains(6))).get
      val node = area.edges.find(_.connection == 3).get._2
      node.corridor should be (5)

    }

  }

  private def findNode(g: Graph[CollapsedNode, CollapsedEdge], n: RoomVertex): CollapsedNode = g.nodes.find(_.vertexCollection.contains(n.rid)).get
  private def findEdge(g: Graph[CollapsedNode, CollapsedEdge], from: RoomVertex, to: RoomVertex) = g.edges.find(e => e.contains(findNode(g, from)) && e.contains(findNode(g, to))).get.toOuter

}
