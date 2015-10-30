package rldungeon.unit.grammar

import helperClasses.RandomMock
import net.cyndeline.rlgraph.subgraph.isomorphism.{IsomorphicMatch, IsomorphicMapping, ElementEquivalence}
import net.cyndeline.scalarlib.rldungeon.grammar.ComponentProduction
import net.cyndeline.scalarlib.rldungeon.grammar.production.SingleProduction
import net.cyndeline.scalarlib.rldungeon.grammar.util.{Morphism, MorphismFactory}
import rldungeon.help.{CorridorEdge, GraphLevel, RoomVertex}
import testHelpers.SpecImports
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

class SingleProductionSpec extends SpecImports {

  describe("SingleProduction") {

    it ("should look for an isomorphic match using a pattern, graph, matcher and negative condition") {

      Given("a SingleProduction looking for a pattern using a matcher, with a negative condition, and an isomorphic matcher")
      val room1 = new RoomVertex(1)
      val room2 = new RoomVertex(2)
      val room4 = new RoomVertex(4)
      val room5 = new RoomVertex(5)
      val room6 = new RoomVertex(6)
      val room7 = new RoomVertex(7)
      val room8 = new RoomVertex(8)
      val room9 = new RoomVertex(9)

      val pattern = Graph[Int, UnDiEdge](1, 2, 3)

      // Misc values that must be supplied
      val componentMock = mock[ComponentProduction[GraphLevel, RoomVertex, CorridorEdge, Int]]
      val morphismMock = mock[MorphismFactory]
      val random = RandomMock()

      When("applying the production onto a level")
      val level = GraphLevel(Graph[RoomVertex, CorridorEdge](room4, room5, room6))

      Then("the isomorphism inspector should receive the pattern, graph, matcher, randomizer and negative condition")
      val returnedMapping = Map[Int, RoomVertex](1 -> room2) // Doesn't matter what this map contains
      val isoMappingMock = mock[IsomorphicMapping[RoomVertex, CorridorEdge, Int, UnDiEdge]]
      isoMappingMock.randomIsomorphicMapping _ expects(level.asGraph, pattern, random) returns Some(new IsomorphicMatch(returnedMapping)) once()

      And("the morphism factory should receive the mapping produced by the isomorphism mapper")
      val morphism = new Morphism(returnedMapping)
      morphismMock.build[RoomVertex, Int] _ expects returnedMapping returns morphism once()

      And("the ComponentProduction should receive the pattern, morphism and graph to apply production onto")
      val result = GraphLevel(Graph[RoomVertex, CorridorEdge](room7, room8, room9))
      componentMock.apply _ expects(morphism, level) returns result once()

      /* Execute test */
      val singleProduction = SingleProduction[GraphLevel, RoomVertex, CorridorEdge, Int, UnDiEdge](pattern, componentMock, random, isoMappingMock, morphismMock)
      singleProduction.apply(level) should equal (Some(result))

    }
  }
}
