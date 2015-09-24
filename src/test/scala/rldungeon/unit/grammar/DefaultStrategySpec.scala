package rldungeon.unit.grammar

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalamock.scalatest.MockFactory
import scala.util.Random
import net.cyndeline.scalarlib.rldungeon.grammar.production.LevelProduction
import net.cyndeline.scalarlib.rldungeon.grammar.DefaultStrategy
import scalax.collection.immutable.Graph
import rldungeon.help.{GraphLevel, RoomVertex, CorridorEdge}

@RunWith(classOf[JUnitRunner])
class DefaultStrategySpec extends FunSpec with GivenWhenThen with ShouldMatchers with MockFactory {

  describe("DefaultStrategy") {

    it ("should select a production at random and apply it to a graph") {

      Given("a default strategy with three productions that selects 2 productions, and a randomizer that selects the first and third production")
      val random = mock[Random]
      val production1 = mock[LevelProduction[GraphLevel, RoomVertex, CorridorEdge]]
      val production2 = mock[LevelProduction[GraphLevel, RoomVertex, CorridorEdge]]
      val production3 = mock[LevelProduction[GraphLevel, RoomVertex, CorridorEdge]]
      val attempts = 1
      val derivations = 2

      val productions = Vector(production1, production2, production3)
      val strategy = new DefaultStrategy(random, productions, derivations, attempts, Set[RoomVertex]())

      (random.nextInt(_: Int)) expects(productions.size) returns (0) once()
      (random.nextInt(_: Int)) expects(productions.size) returns (2) once()
      (random.nextGaussian _) expects() returns (1.0) once()

      val room1 = new RoomVertex(1)
      val room2 = new RoomVertex(2)
      val room3 = new RoomVertex(3)

      When("executing the strategy, with the first production returning a graph with a vertex 1")
      val initialLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](room1))
      val resultFromFirstProduction = GraphLevel(Graph[RoomVertex, CorridorEdge](room2))
      (production1.apply _) expects(initialLevel) returns (Option(resultFromFirstProduction)) once()

      Then("the third production should receive the produced graph")
      val resultFromThirdProduction = GraphLevel(Graph[RoomVertex, CorridorEdge](room2, room3))
      (production3.apply _) expects(resultFromFirstProduction) returns (Option(resultFromThirdProduction)) once()

      /* Execute test. */
      val result = strategy.apply(initialLevel)

      And("the resulting graph should be the result from the third production")
      result should equal (Some(resultFromThirdProduction))

    }

  }
}
