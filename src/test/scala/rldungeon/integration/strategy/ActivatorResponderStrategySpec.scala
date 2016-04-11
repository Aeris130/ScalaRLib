package rldungeon.integration.strategy

import net.cyndeline.rlgraph.pathfinding.Path
import net.cyndeline.rlgraph.util.GraphCommons
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.{CollapsedEdge, CollapsedNode}
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea.ActivatorResponderStrategy
import net.cyndeline.scalarlib.rldungeon.levelPath.TreePath
import rldungeon.help._
import testHelpers.SpecImports

import scala.language.{implicitConversions, postfixOps}
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class ActivatorResponderStrategySpec extends SpecImports {
  private implicit def undirectedToCorridorEdge(e: UnDiEdge[RoomVertex]): CorridorEdgeAssoc[RoomVertex] = new CorridorEdgeAssoc(e)
  private val strategy = new ActivatorResponderStrategy[GraphLevel, RoomVertex, CorridorEdge]()

  private def mainPath = new {
    val start = new RoomVertex(1) // Start
    val r2 = new RoomVertex(2)
    val r3 = new RoomVertex(3)
    val r4 = new RoomVertex(4)
    val r5 = new RoomVertex(5)
    val goal = new RoomVertex(6) // Goal

    val edge1 = CorridorEdge(start, r2)
    val edge2 = CorridorEdge(r2, r3)
    val edge3 = CorridorEdge(r3, r4)
    val edge4 = CorridorEdge(r4, r5)
    val edge5 = CorridorEdge(r5, goal)

    val path = Graph[RoomVertex, CorridorEdge](edge1, edge2, edge3, edge4, edge5)
    val edges = GraphCommons.outerEdges(path).toSet
  }

  /* A main path with a pointless area connected to the start room. */
  private def pointlessAreaConnectedToStart = new {
    val main = mainPath
    import main._

    val pr1 = new RoomVertex(11)
    val pr2 = new RoomVertex(12)
    val pr3 = new RoomVertex(13)

    val area = Graph[RoomVertex, CorridorEdge](pr1~pr2 empty, pr2~pr3 empty)

    val level = GraphLevel(main.path ++ area + (start~pr1 empty), start, goal)
  }

  private def pointlessAreaConnectedToGoal = new {
    val main = mainPath
    import main._

    val pr1 = new RoomVertex(11)
    val pr2 = new RoomVertex(12)
    val pr3 = new RoomVertex(13)

    val area = Graph[RoomVertex, CorridorEdge](pr1~pr2 empty, pr2~pr3 empty)

    val level = GraphLevel(main.path ++ area + (goal~pr1 empty), start, goal)
  }

  /* A pointless area that forks in two after the first edge. */
  private def pointlessAreaWithSplit = new {
    val main = mainPath
    import main._

    val pr1 = new RoomVertex(11)
    val pr2 = new RoomVertex(12)
    val pr3 = new RoomVertex(13)
    val pr4 = new RoomVertex(14)

    val area = Graph[RoomVertex, CorridorEdge](pr1~pr2 empty, pr2~pr3 empty, pr2~pr4 empty)
    val level = GraphLevel(main.path ++ area + (r3~pr1 empty), start, goal)
  }

  private def multiplePointlessAreas = new {
    val main = mainPath
    import main._

    val pr1 = new RoomVertex(11)
    val pr2 = new RoomVertex(12)

    val level = GraphLevel(main.path + (r2~pr1 empty) + (r3~pr2 empty), start, goal)
  }

  // Pointless area connected to start node
  private def mainPathWithDirectedEdge = new {
    val start = new RoomVertex(1) // Start
    val r2 = new RoomVertex(2)
    val r3 = new RoomVertex(3)
    val goal = new RoomVertex(4) // Goal

    val r5 = new RoomVertex(5)

    val edge1 = CorridorEdge(start, r2)
    val edge2 = DiCorridorEdge(r2, r3)
    val edge3 = CorridorEdge(r3, goal)
    val edge4 = CorridorEdge(start, r5)

    val path = Graph[RoomVertex, CorridorEdge](edge1, edge2, edge3) + edge4
    val edges = GraphCommons.outerEdges(path).toSet
    val level = GraphLevel(path, start, goal)
  }


  describe("ActivatorResponderStrategy") {

    it ("should place responders on the edge connected to the start area") {

      Given("a level with a pointless area connected to the first room in the main path")
      val f = pointlessAreaConnectedToStart
      import f._
      import f.main._

      When("placing an activator/responder pair")
      val resultingLevel = strategy.apply(level.setResponderAmount(1)).get

      Then("the responder should be placed in the edge closest to the first vertex")
      resultingLevel.activatorAndResponders should have size 1
      resultingLevel.activatorAndResponders.head._2 should be (CorridorEdge(start, r2))

      And("the activator should be placed on the furthest room in the pointless area")
      resultingLevel.activatorAndResponders.head._1 should be (Set(pr3))

    }

    it ("should not place activators on areas connected to the goal area") {

      Given("a level with a pointless area connected to the last room in the main path")
      val f = pointlessAreaConnectedToGoal
      import f._

      When("placing an activator/responder pair")
      val resultingLevel = strategy.apply(level.setResponderAmount(1)).get

      Then("no activator/responder pair should be placed")
      resultingLevel.activatorAndResponders should be ('empty)

    }

    it ("should place multiple responder and activators in the same pointless area") {

      Given("a level with a pointless area that forks in two towards two different end rooms")
      val f = pointlessAreaWithSplit
      import f._
      val pointlessRoom1 = pr3
      val pointlessRoom2 = pr4

      When("placing two activator/responder pairs")
      val resultingLevel = strategy.apply(level.setResponderAmount(2)).get

      Then("both pointless rooms should have an activator")
      resultingLevel.activatorAndResponders should have size 2
      assert(resultingLevel.activatorAndResponders.exists(_._1 == Set(pointlessRoom1)))
      assert(resultingLevel.activatorAndResponders.exists(_._1 == Set(pointlessRoom2)))

      And("one of the responders should either be on the edge connecting one of the rooms")
      val edgeCandidates = Set(CorridorEdge(pr2, pointlessRoom1), CorridorEdge(pr2, pointlessRoom2))
      resultingLevel.activatorAndResponders.count(e => edgeCandidates.contains(e._2)) should be (1)

      And("one of the responders should be on the main path")
      resultingLevel.activatorAndResponders.count(e => f.main.edges.contains(e._2)) should be (1)

    }

    it ("should place an activator in every pointless areas if possible when there's more than one") {

      Given("a level with two pointless rooms, one connected to room2 and one to room 3 along the main path")
      val f = multiplePointlessAreas
      import f._
      import f.main._

      When("placing two activator/responder pairs")
      val resultingLevel = strategy.apply(level.setResponderAmount(2)).get

      Then("both pointless rooms should have an activator")
      resultingLevel.activatorAndResponders should have size 2
      assert(resultingLevel.activatorAndResponders.exists(entry => entry._1 == Set(pr1)))
      assert(resultingLevel.activatorAndResponders.exists(entry => entry._1 == Set(pr2)))

      And("the edges after room 2 and 3 on the main path should contain responders")
      assert(resultingLevel.activatorAndResponders.exists(entry => entry._2 == edge2))
      assert(resultingLevel.activatorAndResponders.exists(entry => entry._2 == edge3))

    }

    it ("should not place activator/responders if no valid edge is found") {

      Given("a main path with no valid edges and two pointless areas")
      val f = multiplePointlessAreas
      import f._
      import f.main._
      val levelWithPermissions = level
        .setResponderCapacity(edge1, 0)
        .setResponderCapacity(edge2, 0)
        .setResponderCapacity(edge3, 0)
        .setResponderCapacity(edge4, 0)
        .setResponderCapacity(edge5, 0)

      When("placing two activator/responder pairs")
      val resultingLevel = strategy.apply(levelWithPermissions.setResponderAmount(2)).get

      Then("no activator/responders should be placed")
      resultingLevel.activatorAndResponders should be ('empty)

    }

    it ("should not place activator/responders if the start and goal rooms are the same") {

      Given("a main path with a single room, and a pointless area connected to it")
      val f = mainPath
      import f._
      val g = Graph[RoomVertex, CorridorEdge](start~r2 empty)
      val level = GraphLevel(g, start, start)

      When("placing activator/responder pairs")
      val resultingLevel = strategy.apply(level.setResponderAmount(2)).get

      Then("no activator/responders should be placed")
      resultingLevel.activatorAndResponders should be ('empty)

    }

    it ("should not consider biconnected sections sharing a vertex as cutpoint to be eligible for responders between them") {

      Given("a level with two biconnected components sharing a vertex on the main path")
      val f = mainPath
      import f._

      val c1 = Graph[RoomVertex, CorridorEdge](start~r2 empty, r2~r3 empty, r3~start empty)
      val c2 = Graph[RoomVertex, CorridorEdge](r3~r4 empty, r4~goal empty, goal~r3 empty)
      val pointlessArea = Graph[RoomVertex, CorridorEdge](r2~r5 empty)
      val g = c1 ++ c2 ++ pointlessArea
      val level = GraphLevel(g, start, goal)

      When("placing activator/responder pairs")
      val resultingLevel = strategy.apply(level.setResponderAmount(2)).get

      Then("no activator/responders should be placed")
      resultingLevel.activatorAndResponders should be ('empty)

    }

    it ("should not place responders on the path between a main area connection outside the initial main path, and the" +
      "original main area connection on the main path") {

      Given("a level with an initial main path 1 -> 6, and a pointless area 2-7, 7-8, 7-9 where every edge can carry responders")
      val f = mainPath
      import f._

      val r7 = new RoomVertex(7)
      val r8 = new RoomVertex(8)
      val r9 = new RoomVertex(9)
      val pe1 = r7~r8 empty
      val pe2 = r7~r9 empty
      val pointlessArea = Graph[RoomVertex, CorridorEdge](r2~r7 empty, pe1, pe2)
      val g = path ++ pointlessArea
      val level = GraphLevel(g, start, goal)

      When("placing 2 activator/responder pairs")
      val resultingLevel = strategy.apply(level.setResponderAmount(2)).get

      Then("the first responder should be placed between 2 and 3")
      resultingLevel.activatorAndResponders.map(_._2) should contain (edge2)

      And("the second responder should be placed between either 7 and 8 or 7 and 9, depending on which room that got " +
        "the first activator")

      /* If the edge 2-3 has its activator on node 8, then 7-8 is now on the main path and will receive the activator
       * for node 9. Vice versa if the first activator ended up on node 9.
       */
      val firstActivatorRoom: RoomVertex = resultingLevel.activatorAndResponders.find(_._2 == edge2).get._1.head
      val secondPointlessEdge = if (firstActivatorRoom == r8) pe1 else pe2
      resultingLevel.activatorAndResponders.map(_._2) should contain (secondPointlessEdge)

    }

    it ("should place responders on directed edges") {

      Given("a main path 1-2->3-4, where the edge 1-2 cannot hold responders")
      val f = mainPathWithDirectedEdge
      import f._
      val levelWithResponderRestrictions = level.setResponderCapacity(edge1, 0)

      When("placing 1 activator/responder pair using node 1 as the main area connection")
      val resultingLevel = strategy.apply(levelWithResponderRestrictions.setResponderAmount(1)).get

      Then("the directed edge 2->3 should receive a responder")
      resultingLevel.activatorAndResponders should have size 1
      resultingLevel.activatorAndResponders.head._1.head should be (r5)
      resultingLevel.activatorAndResponders.head._2 should be (edge2)

    }

    it("should not place responders beyond a directed edge along the original main path") {

      Given("a main path 1-2->3-4, where edges 1-2 and 2-3 cannot hold responders")
      val f = mainPathWithDirectedEdge
      import f._
      val levelWithResponderRestrictions = level.setResponderCapacity(edge1, 0).setResponderCapacity(edge2, 0)

      When("placing 1 activator/responder pair using node 1 as the main area connection")
      val resultingLevel = strategy.apply(levelWithResponderRestrictions.setResponderAmount(1)).get

      Then("no activator/responder pair should be placed")
      resultingLevel.activatorAndResponders should be ('empty)

    }

    it ("should place responders on a directed edge that has another directed edge going in the opposite direction") {

      Given("a path 1<->2<->3<->4 with three pointless areas connected to room 1")
      val start = new RoomVertex(1) // Start
      val r2 = new RoomVertex(2)
      val r3 = new RoomVertex(3)
      val goal = new RoomVertex(4) // Goal

      val r5 = new RoomVertex(5)
      val r6 = new RoomVertex(6)
      val r7 = new RoomVertex(7)

      val edge1 = DiCorridorEdge(start, r2)
      val edge1_2 = DiCorridorEdge(r2, start)
      val edge2 = DiCorridorEdge(r2, r3)
      val edge2_2 = DiCorridorEdge(r3, r2)
      val edge3 = DiCorridorEdge(r3, goal)
      val edge3_2 = DiCorridorEdge(goal, r3)

      val edge4 = CorridorEdge(start, r5)
      val edge5 = CorridorEdge(start, r6)
      val edge6 = CorridorEdge(start, r7)

      val path = Graph[RoomVertex, CorridorEdge](edge1, edge1_2, edge2, edge2_2, edge3, edge3_2) + edge4 + edge5 + edge6
      val edges = GraphCommons.outerEdges(path).toSet

      // No responders allowed outside the initial main path
      val level = GraphLevel(path, start, goal)
        .setResponderCapacity(edge4, 0)
        .setResponderCapacity(edge5, 0)
        .setResponderCapacity(edge6, 0)

      When("placing 3 activator/responder pairs using node 1 as the main area connection")
      val resultingLevel = strategy.apply(level.setResponderAmount(3)).get

      Then("1->2, 2->3 and 3->4 should receive a responder, and not the ones going in the opposite direction")
      val edgesWithResponders = resultingLevel.activatorAndResponders.map(_._2)
      edgesWithResponders should have size 3
      edgesWithResponders should contain (edge1)
      edgesWithResponders should contain (edge2)
      edgesWithResponders should contain (edge3)

    }

    /*
     * Main paths
     */

    // This test isn't really exhaustive, it only verifies that the data used to construct the main path is based on
    // modifications of the level, and not its initial path.
    it ("should update the level object with the final main path") {

      Given("a level whose final main path will be 1, 2, 4, 2, 3")
      val start = new RoomVertex(1) // Start
      val r2 = new RoomVertex(2)
      val goal = new RoomVertex(3)
      val r4 = new RoomVertex(4) // Pointless area
      val graph = Graph[RoomVertex, CorridorEdge](start~r2 cid 1, r2~goal cid 2, r2~r4 cid 3)

      val n1 = new CollapsedNode(start.rid)
      val n2 = new CollapsedNode(r2.rid)
      val n3 = new CollapsedNode(goal.rid)
      val n4 = new CollapsedNode(r4.rid)
      val ce1 = CollapsedEdge(n1, n2)
      val ce2 = CollapsedEdge(n2, n3)
      val ce3 = CollapsedEdge(n2, n4)
      val cg = Graph(ce1, ce2, ce3)

      val mainPath = Path(n1, Vector(ce1, ce3, ce3, ce2))
      val level = GraphLevel(graph, start, goal)

      When("placing activator/responder pairs")
      val resultingLevel = strategy.apply(level.setResponderAmount(1)).get
      val finalPath = resultingLevel.mainPath.get

      Then("the final registered main path should be 1, 2, 4, 2, 3")
      val expectedTreePath = TreePath(level, cg, mainPath)
      finalPath should be (expectedTreePath)

    }

  }
}

