package rldungeon.unit.grammar

import net.cyndeline.scalarlib.rldungeon.grammar.production.Production
import rldungeon.help.{CorridorEdge, GraphLevel, RoomVertex}
import testHelpers.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

class ProductionSpec extends SpecImports {

  describe("Production") {

    it ("should throw an exception if an empty pattern is used") {

      Given("a class that extends Production")
      class SomeClass(pattern: Graph[Int, UnDiEdge]) extends Production[GraphLevel, RoomVertex, CorridorEdge, Int, UnDiEdge](pattern, null, null, null) {
        def apply(level: GraphLevel): Option[GraphLevel] = Some(level)
      }

      When("instantiating the class with an empty pattern")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new SomeClass(Graph[Int, UnDiEdge]())
      }

    }
  }
}
