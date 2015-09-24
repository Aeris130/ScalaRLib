package rldungeon.unit.grammar

import helperClasses.RandomMock
import net.cyndeline.rlcommon.util.RandomCollection
import net.cyndeline.rlgraph.subgraph.isomorphism.{ElementEquivalence, NegativeCondition}
import net.cyndeline.scalarlib.rldungeon.grammar.ComponentProduction
import net.cyndeline.scalarlib.rldungeon.grammar.production.MultiProduction
import net.cyndeline.scalarlib.rldungeon.grammar.util.{Morphism, MorphismFactory}
import rldungeon.help.{CorridorEdge, GraphLevel, IsomorphicMappingMock, RoomVertex}
import testHelpers.SpecImports

import scalax.collection.immutable.Graph

class MultiProductionSpec extends SpecImports {

  describe("MultiProduction") {

    it ("it should look for a topology match and select a production randomly") {

      Given("a MultiProduction with a random collection")
      val room1 = new RoomVertex(1)
      val room2 = new RoomVertex(2)
      val room3 = new RoomVertex(3)
      val room4 = new RoomVertex(4)
      val room5 = new RoomVertex(5)
      val room6 = new RoomVertex(6)
      val room7 = new RoomVertex(7)
      val room8 = new RoomVertex(8)
      val room9 = new RoomVertex(9)

      val pattern = Graph[RoomVertex, CorridorEdge](room1, room2, room3)
      val matcherMock = mock[ElementEquivalence[RoomVertex, CorridorEdge]]
      val negativeConditionMock = mock[NegativeCondition[RoomVertex, CorridorEdge]]

      // Misc values that must be supplied
      val componentMock = mock[ComponentProduction[GraphLevel, RoomVertex, CorridorEdge]]
      val morphismMock = mock[MorphismFactory]
      val random = RandomMock()

      val randomCollection = mock[RandomCollection[ComponentProduction[GraphLevel, RoomVertex, CorridorEdge]]]

      When("applying the production onto a level")
      val level = GraphLevel(Graph[RoomVertex, CorridorEdge](room4, room5, room6))

      Then("the isomorphism inspector should receive the pattern, graph, matcher, randomizer and negative condition")
      val returnedMapping = Map[RoomVertex, RoomVertex](room1 -> room2) // Doesn't matter what this map contains
      val expectedInData = ((pattern, level.asGraph, matcherMock, random, Option(negativeConditionMock)), returnedMapping)
      val customMock = new IsomorphicMappingMock[RoomVertex, CorridorEdge](Vector(expectedInData) )

      And("the morphism factory should receive the mapping produced by the isomorphism mapper")
      val morphism = new Morphism(returnedMapping)
      (morphismMock.build[RoomVertex](_)) expects(returnedMapping) returns(morphism) once()

      And("the random collection should be built using the supplied component productions")
      val productions = Vector[(ComponentProduction[GraphLevel, RoomVertex, CorridorEdge], Double)]((componentMock, 1.0))
      (randomCollection.add _) expects(1.0, componentMock) returns() once()

      And("the random collection should be polled for a ComponentProduction once")
      (randomCollection.next _) expects() returns(componentMock) once()

      And("the ComponentProduction should receive the pattern, morphism and graph to apply production onto")
      val result = GraphLevel(Graph[RoomVertex, CorridorEdge](room7, room8, room9))
      (componentMock.apply _) expects(morphism, level) returns (result) once()

      /* Execute test */
      val multiProduction = new MultiProduction(pattern, matcherMock, negativeConditionMock, productions, random, customMock, morphismMock, randomCollection)
      multiProduction.apply(level) should equal (Some(result))

      assert(customMock.hasMetExpectations)

    }
  }
}
