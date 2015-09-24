package rldungeon.unit.strategy

import net.cyndeline.scalarlib.rldungeon.dgs.strategy.LevelStrategy
import net.cyndeline.scalarlib.rldungeon.grammar.Strategy
import rldungeon.help.{CorridorEdge, GraphLevel, RoomVertex}
import testHelpers.SpecImports

import scalax.collection.immutable.Graph

class LevelStrategySpec extends SpecImports {

  describe("LevelStrategy") {

    it ("should call the supplied strategies in order") {

      Given("a level strategy with three strategies in the order A, B, C")
      val A = mock[Strategy[GraphLevel, RoomVertex, CorridorEdge]]
      val B = mock[Strategy[GraphLevel, RoomVertex, CorridorEdge]]
      val C = mock[Strategy[GraphLevel, RoomVertex, CorridorEdge]]
      val strategy = new LevelStrategy[GraphLevel, RoomVertex, CorridorEdge](Vector(A, B, C))

      When("applying the strategy to a graph")
      val graph = GraphLevel(Graph[RoomVertex, CorridorEdge]())

      Then("the apply method of each strategy should be invoked in the order they area passed into the strategy")
      inSequence {
        (A.apply _) expects(*) returns (Some(graph)) once()
        (B.apply _) expects(*) returns (Some(graph)) once()
        (C.apply _) expects(*) returns (Some(graph)) once()
      }

      strategy.apply(graph)

    }

    it ("should return None if a strategy fails to produce a result") {

      Given("a level strategy with three strategies in the order A, B, C")
      val A = mock[Strategy[GraphLevel, RoomVertex, CorridorEdge]]
      val B = mock[Strategy[GraphLevel, RoomVertex, CorridorEdge]]
      val C = mock[Strategy[GraphLevel, RoomVertex, CorridorEdge]]
      val strategy = new LevelStrategy[GraphLevel, RoomVertex, CorridorEdge](Vector(A, B, C))
      val graph = GraphLevel(Graph[RoomVertex, CorridorEdge]())

      When("strategy B returns None")
      (A.apply _) expects(*) returns (Some(graph)) once()
      (B.apply _) expects(*) returns (None) once()

      Then("the result should be None and C should not be called")
      strategy.apply(graph) should be (None)

    }

    it ("should use the result of a strategy as input to the next") {

      Given("a level strategy with three strategies in the order A, B, C")
      val A = mock[Strategy[GraphLevel, RoomVertex, CorridorEdge]]
      val B = mock[Strategy[GraphLevel, RoomVertex, CorridorEdge]]
      val C = mock[Strategy[GraphLevel, RoomVertex, CorridorEdge]]
      val strategy = new LevelStrategy[GraphLevel, RoomVertex, CorridorEdge](Vector(A, B, C))
      val initialGraph = GraphLevel(Graph[RoomVertex, CorridorEdge]())

      When("A receive the initial graph and returns graph 1")
      val g1 = GraphLevel(Graph[RoomVertex, CorridorEdge](new RoomVertex(1)))
      val g2 = GraphLevel(Graph[RoomVertex, CorridorEdge](new RoomVertex(2)))
      (A.apply _) expects(initialGraph) returns (Some(g1)) once()

      Then("B should receive A's output as input")
      (B.apply _) expects(g1) returns (Some(g2)) once()

      And("C should receive B's output as input")
      (C.apply _) expects(g2) returns (None) once()

      strategy.apply(initialGraph)

    }

  }

}
