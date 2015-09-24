package rldungeon.unit.grammar

import net.cyndeline.scalarlib.rldungeon.grammar.util.{Morphism, RoomModification, TopologyProduction}
import rldungeon.help._
import testHelpers.SpecImports

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class TopologyProductionSpec extends SpecImports {
  private implicit def edge2CorridorEdgeAssoc(e: UnDiEdge[RoomVertex]) = CorridorEdge(e._1, e._2)

  /**
   * Some pre-constructed vertices.
   */
  def vertices = new {
    val v1 = new RoomVertex(1)
    val v2 = new RoomVertex(2)
    val v3 = new RoomVertex(3)
    val v4 = new RoomVertex(4)
    val v5 = new RoomVertex(5)
    val v6 = new RoomVertex(6)
    val v7 = new RoomVertex(7)
    val v8 = new RoomVertex(8)

    val nextRoomId = 9
  }

  describe("TopologyProduction") {

    it ("should throw an exception when attempting to add edges between old vertices that is scheduled for deletion") {

      Given("a component production that takes a graph with two old vertices, adds an edge between them then deletes one vertex")
      val v1 = new RoomVertex(1)
      val removable = new RoomVertex(0)

      When("constructing the production")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()
          .addOldEdge(v1, removable)
          .removeVertex(removable)
      }

    }

    it ("should throw an exception when attempting to add edges between a new and an old vertex that is scheduled for deletion") {

      Given("a component production that takes a graph with two old vertices, adds an edge between them then deletes one vertex")
      val removable = new RoomVertex(0)

      When("constructing the production")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()
          .addMixEdge(1, removable)
          .removeVertex(removable)
      }

    }

    it ("should throw an exception if a vertex identifier is used twice when adding vertices") {

      Given("an empty component production")
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()

      When("adding the same vertex twice")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        prod
          .addVertex(1)
          .addVertex(1)
      }
    }

    it ("should throw an exception when attempting to add edges to and from the same new vertex") {

      Given("an empty component production")
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()

      When("adding an edge to and from the same new vertex")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        prod.addNewEdge(1, 1)
      }
    }

    it ("should throw an exception when attempting to add edges to and from the same old vertex") {

      Given("an empty component production")
      val v1 = new RoomVertex(1)
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()

      When("adding an edge to and from the same old vertex")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        prod.addOldEdge(v1, v1)
      }
    }

    it ("should throw an exception if attempting to both add and remove an edge between two old vertices") {

      Given("an empty component production")
      val f = vertices
      import f._
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()

      When("adding and removing the same edge")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        prod
          .addOldEdge(v1, v2)
          .removeEdge(v1, v2)
      }
      intercept[IllegalArgumentException] {
        prod
          .addOldEdge(v1, v2)
          .removeEdge(v2, v1)
      }

    }

    it ("should throw an exception when adding a vertex modification to a vertex id that isn't present") {

      Given("a component productions")
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()

      When("adding a vertex modification to an id that hasn't been added")
      Then("an exception should be thrown")
      intercept[Error] {
        prod.addModification(1, null)
      }

    }

    it ("should add a vertex to a graph") {

      Given("a component production that adds a vertex to a graph")
      val vertexToAdd = new RoomVertex(0)

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()
        .addVertex(1)

      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      val morphism = new Morphism[RoomVertex](Map())

      When("applying the production to an empty graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should contain the new vertex.")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](vertexToAdd))

    }

    it ("should add an edge between two previous vertices") {

      Given("a component production that adds an edge to a graph")
      val f = vertices
      import f._

      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v3, v4))
      val morphism = new Morphism[RoomVertex](Map(v1 -> v3, v2 -> v4))

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()
        .addOldEdge(v1, v2)

      When("applying the production to an empty graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should contain an edge between v3 and v4")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](v3~v4))

    }

    it ("should add an edge between a previous vertex and a new vertex") {

      Given("a component that adds a new vertex to a graph with an old vertex, then adds an edge between them")
      val f = vertices
      import f._
      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v3))
      val vertexToAdd = new RoomVertex(v3.rid + 1)
      val morphism = new Morphism[RoomVertex](Map(v1 -> v3))

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()
        .addVertex(1)
        .addMixEdge(1, v1)

      When("applying the production to the graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the result should contain an edge between v3 and the added edge")
      result.asGraph should equal (Graph(CorridorEdge(v3, vertexToAdd)))

    }

    it ("should add an edge between two new vertices") {

      Given("a production that adds two vertices and adds an edge between them")
      val vertexToAdd1 = new RoomVertex(0)
      val vertexToAdd2 = new RoomVertex(1)
      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      val morphism = new Morphism[RoomVertex]()

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()
        .addVertex(1)
        .addVertex(2)
        .addNewEdge(1, 2)

      When("applying the production to the graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should have an edge between the two new vertices")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](vertexToAdd1~vertexToAdd2))

    }

    it ("should remove vertices") {

      Given("a graph with two vertices v1, v2 and a production that removes v1")
      val f = vertices
      import f._
      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v3, v4))
      val morphism = new Morphism[RoomVertex](Map(v1 -> v3, v2 -> v4))

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()
        .removeVertex(v2)

      When("applying the production to the graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should only contain v3")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](v3))

    }

    it ("should apply all rules in the same production") {

      Given("a graph with 4 vertices and a production that performs every modification")
      val f = vertices
      import f._
      val pattern = Graph[RoomVertex, CorridorEdge](v1~v2, v1~v3, v3~v4) // Not used, just displayed for readability
      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v5~v6, v5~v7, v7~v8))
      val morphism = new Morphism[RoomVertex](Map(v1 -> v5, v2 -> v6, v3 -> v7, v4 -> v8))
      val vertexToAdd1 = new RoomVertex(9)
      val vertexToAdd2 = new RoomVertex(10)

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()
        .addVertex(1) // Adds new vertex
        .addVertex(2)
        .addNewEdge(1, 2) // Adds an edge between two new vertices
        .addMixEdge(2, v1) // Adds an edge between a new and an old vertex
        .addOldEdge(v1, v4) // Adds an edge between two old vertices
        .removeVertex(v2) // Removes v3 and edges v1~v2
        .removeEdge(v1, v3) // removes edge between v1 and v3

      When("applying the production to the graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the result should have all rules applied")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](vertexToAdd1~vertexToAdd2, v5~vertexToAdd2, v7~v8, v5~v8))

    }

    it ("should modify vertices as they are created") {

      Given("a production that adds a new vertex to the graph and a modification that produces another vertex based on the id of the first")
      val f = vertices
      import f._
      val level = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      val morphism = new Morphism[RoomVertex](Map())

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()
        .addVertex(1)
        .addModification(1, new RoomModification[RoomVertex]() {
          def modify(room: RoomVertex): RoomVertex = {
            v2 // changes a vertex into v2
          }
      })

      When("applying the production to the graph")
      val resultingGraph = prod.apply(morphism, level)

      Then("the new graph should contain v2")
      resultingGraph.asGraph should be (Graph[RoomVertex, CorridorEdge](v2))

    }

    it ("should only modify a subset of the added vertex ids") {

      Given("a production that adds a two new vertices to the graph and a modification that produces another vertex based on the id of the first")
      val f = vertices
      import f._
      val level = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      val morphism = new Morphism[RoomVertex](Map())

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge]()
        .addVertex(1)
        .addVertex(2)
        .addModification(1, new RoomModification[RoomVertex]() {
        def modify(room: RoomVertex): RoomVertex = {
          v3 // changes a vertex v1 into v3
        }
      })

      When("applying the production to the graph")
      val resultingGraph = prod.apply(morphism, level)

      Then("the new graph should contain v3 and a room with id 1 (2 - 1, as the first room gets id 0)")
      resultingGraph.asGraph should be (Graph[RoomVertex, CorridorEdge](v1, v3))

    }



  }
}
