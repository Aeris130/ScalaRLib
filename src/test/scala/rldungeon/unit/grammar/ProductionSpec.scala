package rldungeon.unit.grammar

import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.cyndeline.scalarlib.rldungeon.grammar.production.Production
import scalax.collection.immutable.Graph
import rldungeon.help.{GraphLevel, CorridorEdge, RoomVertex}
import net.cyndeline.scalarlib.rldungeon.common.Level

@RunWith(classOf[JUnitRunner])
class ProductionSpec extends FunSpec with GivenWhenThen with ShouldMatchers {

  describe("Production") {

    it ("should throw an exception if an empty pattern is used") {

      Given("a class that extends Production")
      class SomeClass(pattern: Graph[RoomVertex, CorridorEdge]) extends Production[GraphLevel, RoomVertex, CorridorEdge](pattern, null, None, null, null, null) {
        def apply(level: GraphLevel): Option[GraphLevel] = ???
      }

      When("instantiating the class with an empty pattern")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new SomeClass(Graph[RoomVertex, CorridorEdge]())
      }

    }
  }
}
