package rldungeon.unit.grammar

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalamock.scalatest.MockFactory
import net.cyndeline.scalarlib.rlgraph.subgraph.isomorphism.{NegativeCondition, ElementEquivalence}
import net.cyndeline.scalarlib.rldungeon.grammar.ComponentProduction
import net.cyndeline.scalarlib.rldungeon.grammar.util.{Morphism, MorphismFactory}
import scalax.collection.immutable.Graph
import net.cyndeline.scalarlib.rldungeon.grammar.production.SingleProduction
import scala.util.Random
import rldungeon.help.{GraphLevel, CorridorEdge, RoomVertex, IsomorphicMappingMock}

@RunWith(classOf[JUnitRunner])
class SingleProductionSpec extends FunSpec with GivenWhenThen with ShouldMatchers with MockFactory {

  describe("SingleProduction") {

    it ("should look for an isomorphic match using a pattern, graph, matcher and negative condition") {

      Given("a SingleProduction looking for a pattern using a matcher, with a negative condition, and an isomorphic matcher")
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
      val random = mock[Random]

      When("applying the production onto a level")
      val level = GraphLevel(Graph[RoomVertex, CorridorEdge](room4, room5, room6))

      Then("the isomorphism inspector should receive the pattern, graph, matcher, randomizer and negative condition")
      val returnedMapping = Map[RoomVertex, RoomVertex](room1 -> room2) // Doesn't matter what this map contains
      val expectedInData = ((pattern, level.asGraph, matcherMock, random, Option(negativeConditionMock)), returnedMapping)
      val customMock = new IsomorphicMappingMock[RoomVertex, CorridorEdge](Vector(expectedInData) )

      And("the morphism factory should receive the mapping produced by the isomorphism mapper")
      val morphism = new Morphism(returnedMapping)
      (morphismMock.build[RoomVertex](_)) expects(returnedMapping) returns(morphism) once()

      And("the ComponentProduction should receive the pattern, morphism and graph to apply production onto")
      val result = GraphLevel(Graph[RoomVertex, CorridorEdge](room7, room8, room9))
      (componentMock.apply _) expects(morphism, level) returns (result) once()

      /* Execute test */
      val singleProduction = new SingleProduction(pattern, matcherMock, negativeConditionMock, componentMock, random, customMock, morphismMock)
      singleProduction.apply(level) should equal (Some(result))

      assert(customMock.hasMetExpectations)

    }
  }
}
