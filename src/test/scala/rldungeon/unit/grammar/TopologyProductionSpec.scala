package rldungeon.unit.grammar

import net.cyndeline.scalarlib.rldungeon.common.{Undirected, Directed}
import net.cyndeline.scalarlib.rldungeon.grammar.util.{Morphism, RoomModification, TopologyProduction}
import org.scalatest.words.ResultOfATypeInvocation
import rldungeon.help._
import testHelpers.SpecImports

import scala.language.implicitConversions
import scalax.collection.GraphEdge.{DiEdge, UnDiEdge}
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class TopologyProductionSpec extends SpecImports {
  private implicit def edge2CorridorEdgeAssoc(e: UnDiEdge[RoomVertex]): CorridorEdge[RoomVertex] = CorridorEdge(e._1, e._2)
  private implicit def diEdge2DiCorridorEdgeAssoc(e: DiEdge[RoomVertex]): DiCorridorEdge[RoomVertex] = DiCorridorEdge(e._1, e._2)

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

    val pa = "A"
    val pb = "B"
    val pc = "C"
    val pd = "D"
  }

  describe("TopologyProduction") {

    it ("should throw an exception when attempting to add undirected edges between old vertices that is scheduled for deletion") {

      Given("a component production that takes a graph with two old vertices, adds an undirected edge between them then deletes one vertex")
      When("constructing the production")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
          .addOldEdge("A", "B")
          .removeVertex("B")
      }

    }

    it ("should throw an exception when attempting to add directed edges between old vertices that is scheduled for deletion") {

      Given("a component production that takes a graph with two old vertices, adds a directed edge between them then deletes one vertex")
      When("constructing the production")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
          .addOldEdge("A", "B", Directed)
          .removeVertex("B")
      }

    }

    it ("should throw an exception when attempting to add undirected edges between a new and an old vertex that is scheduled for deletion") {

      Given("a component production that takes a graph with two old vertices, adds an edge between them then deletes one vertex")
      val removable = "A"

      When("constructing the production")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
          .addMixEdge(1, removable)
          .removeVertex(removable)
      }

    }

    it ("should throw an exception when attempting to add directed edges between a new and an old vertex that is scheduled for deletion") {

      Given("a component production that takes a graph with two old vertices, adds a directed edge between them then deletes one vertex")
      val removable = "A"

      When("constructing the production")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
          .addMixEdge(1, removable, Directed)
          .removeVertex(removable)
      }
      intercept[IllegalArgumentException] {
        new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
          .addMixEdge(removable, 1, Directed)
          .removeVertex(removable)
      }

    }

    it ("should throw an exception if a vertex identifier is used twice when adding vertices") {

      Given("an empty component production")
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()

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
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()

      When("adding an edge to and from the same new vertex")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        prod.addNewEdge(1, 1)
      }

    }

    it ("should throw an exception when attempting to add edges to and from the same old vertex") {

      Given("an empty component production")
      val old = "A"
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()

      When("adding an edge to and from the same old vertex")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        prod.addOldEdge(old, old)
      }
    }

    it ("should throw an exception if attempting to both add and remove an undirected edge between two old vertices") {

      Given("an empty component production")
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
      val a = "A"
      val b = "B"

      When("adding and removing the same edge")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        prod
          .addOldEdge(a, b)
          .removeEdge(a, b, Undirected)
      }
      intercept[IllegalArgumentException] {
        prod
          .addOldEdge(a, b)
          .removeEdge(b, a, Undirected)
      }

    }

    it ("should throw an exception if attempting to both add and remove a directed edge between two old vertices") {

      Given("an empty component production")
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
      val a = "A"
      val b = "B"

      When("adding and removing the same edge")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        prod
          .addOldEdge(a, b, Directed)
          .removeEdge(a, b, Directed)
      }

    }

    it ("should throw an exception when adding a vertex modification to a vertex id that isn't present") {

      Given("a component productions")
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()

      When("adding a vertex modification to an id that hasn't been added")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        prod.addModification(1, null)
      }

    }

    it ("should throw an exception when adding the same new undirected edge twice") {

      Given("a component productions")
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()

      When("adding an undirected edge twice")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        prod.addNewEdge(1, 2).addNewEdge(1, 2)
      }

    }

    it ("should throw an exception when adding the same new directed edge twice") {

      Given("a component productions")
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()

      When("adding an undirected edge twice")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        prod.addNewEdge(1, 2, Directed).addNewEdge(1, 2, Directed)
      }

    }

    it ("should throw an exception when adding a new directed edge to a production where an undirected edge already exists") {

      Given("a component productions")
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()

      When("adding an undirected edge followed by a directed edge")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        prod.addNewEdge(1, 2).addNewEdge(1, 2, Directed)
      }

    }

    it ("should throw an exception when adding a new undirected edge to a production where a directed edge already exists") {

      Given("a component productions")
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()

      When("adding a directed edge followed by an undirected edge")
      Then("an exception should be thrown")
      intercept[IllegalArgumentException] {
        prod.addNewEdge(1, 2, Directed).addNewEdge(1, 2)
      }

    }

    it ("should add a vertex to a graph") {

      Given("a component production that adds a vertex to a graph")
      val vertexToAdd = new RoomVertex(0)

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .addVertex(1)

      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      val morphism = new Morphism[RoomVertex, String](Map())

      When("applying the production to an empty graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should contain the new vertex.")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](vertexToAdd))

    }

    it ("should add an undirected edge between two previous vertices") {

      Given("a component production that adds an edge to a graph")
      val f = vertices
      import f._

      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v3, v4))
      val morphism = new Morphism[RoomVertex, String](Map(pa -> v3, pb -> v4))

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .addOldEdge(pa, pb)

      When("applying the production to an empty graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should contain an edge between v3 and v4")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](v3~v4))

    }

    it ("should add a directed edge between two previous vertices") {

      Given("a component production that adds a directed edge to a graph")
      val f = vertices
      import f._

      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v3, v4))
      val morphism = new Morphism[RoomVertex, String](Map(pa -> v3, pb -> v4))

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .addOldEdge(pa, pb, Directed)

      When("applying the production to an empty graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should contain a directed edge from v3 to v4")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](v3~>v4))

    }

    it ("should add two directed edges between two previous vertices") {

      Given("a component production that adds two directed edges to a graph")
      val f = vertices
      import f._

      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v3, v4))
      val morphism = new Morphism[RoomVertex, String](Map(pa -> v3, pb -> v4))

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .addOldEdge(pa, pb, Directed)
        .addOldEdge(pb, pa, Directed)

      When("applying the production to an empty graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should contain a directed edge from v3 to v4 and from v4 to v3")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](v3~>v4, v4~>v3))

    }

    it ("should add an undirected edge between a previous vertex and a new vertex") {

      Given("a component that adds a new vertex to a graph with an old vertex, then adds an undirected edge between them")
      val f = vertices
      import f._
      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v3))
      val vertexToAdd = new RoomVertex(v3.rid + 1)
      val morphism = new Morphism[RoomVertex, String](Map(pa -> v3))

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .addVertex(1)
        .addMixEdge(1, pa)

      When("applying the production to the graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the result should contain an edge between v3 and the added edge")
      result.asGraph should equal (Graph(CorridorEdge(v3, vertexToAdd)))

    }

    it ("should add a directed edge between a previous vertex and a new vertex") {

      Given("a component that adds a new vertex to a graph with an old vertex, then adds a directed edge between them")
      val f = vertices
      import f._
      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v3))
      val vertexToAdd = new RoomVertex(v3.rid + 1)
      val morphism = new Morphism[RoomVertex, String](Map(pa -> v3))

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .addVertex(1)
        .addMixEdge(pa, 1, Directed)

      When("applying the production to the graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the result should contain a directed edge between v3 and the added vertex")
      result.asGraph should equal (Graph(DiCorridorEdge(v3, vertexToAdd)))

    }

    it ("should add two directed edges between a previous vertex and a new vertex") {

      Given("a component that adds a new vertex to a graph with an old vertex, then adds two directed edges between them")
      val f = vertices
      import f._
      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v3))
      val vertexToAdd = new RoomVertex(v3.rid + 1)
      val morphism = new Morphism[RoomVertex, String](Map(pa -> v3))

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .addVertex(1)
        .addMixEdge(1, pa, Directed)
        .addMixEdge(pa, 1, Directed)

      When("applying the production to the graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the result should contain a directed edge between v3 and the added edge and in the opposite direction")
      result.asGraph should equal (Graph(DiCorridorEdge(v3, vertexToAdd), DiCorridorEdge(vertexToAdd, v3)))

    }

    it ("should add an undirected edge between two new vertices") {

      Given("a production that adds two vertices and adds an undirected edge between them")
      val vertexToAdd1 = new RoomVertex(0)
      val vertexToAdd2 = new RoomVertex(1)
      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      val morphism = new Morphism[RoomVertex, String]()

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .addVertex(1)
        .addVertex(2)
        .addNewEdge(1, 2)

      When("applying the production to the graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should have an undirected edge between the two new vertices")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](vertexToAdd1~vertexToAdd2))

    }

    it ("should add a directed edge between two new vertices") {

      Given("a production that adds two vertices and adds a directed edge between them")
      val vertexToAdd1 = new RoomVertex(0)
      val vertexToAdd2 = new RoomVertex(1)
      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      val morphism = new Morphism[RoomVertex, String]()

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .addVertex(1)
        .addVertex(2)
        .addNewEdge(1, 2, Directed)

      When("applying the production to the graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should have a directed edge between the two new vertices")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](vertexToAdd1~>vertexToAdd2))

    }

    it ("should add two directed edges between two new vertices") {

      Given("a production that adds two vertices and adds two directed edges between them")
      val vertexToAdd1 = new RoomVertex(0)
      val vertexToAdd2 = new RoomVertex(1)
      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      val morphism = new Morphism[RoomVertex, String]()

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .addVertex(1)
        .addVertex(2)
        .addNewEdge(1, 2, Directed)
        .addNewEdge(2, 1, Directed)

      When("applying the production to the graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should have a directed edge between the two new vertices in both directions")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](vertexToAdd1~>vertexToAdd2, vertexToAdd2~>vertexToAdd1))

    }

    it ("should remove vertices") {

      Given("a graph with two vertices v1, v2 and a production that removes v1")
      val f = vertices
      import f._
      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v3, v4))
      val morphism = new Morphism[RoomVertex, String](Map(pa -> v3, pb -> v4))

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .removeVertex(pb)

      When("applying the production to the graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should only contain v3")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](v3))

    }

    it ("should remove a vertex and an edge connected to the vertex") {

      Given("a graph with the edge v1~v2 and the edge v2~v3, and a production that removes v1 and v1~v2")
      val f = vertices
      import f._
      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v1~v2, v2~v3))
      val morphism = new Morphism[RoomVertex, String](Map(pa -> v1, pb -> v2, pc -> v3))
      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .removeVertex(pa)
        .removeEdge(pa, pb, Undirected)

      When("applying the production to the graph")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should only contain v2~v3")
      result.asGraph should equal (Graph[RoomVertex, CorridorEdge](v2~v3))

    }

    it ("should remove an undirected edge and add a directed instead") {

      Given("a component production that removed an undirected edge and adds a directe edge using the same vertex pair")
      val f = vertices
      import f._

      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v3~v4))
      val morphism = new Morphism[RoomVertex, String](Map(pa -> v3, pb -> v4))

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .removeEdge(pa, pb, Undirected)
        .addOldEdge(pa, pb, Directed)

      When("applying the production to a level with an undirected edge")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should contain an undirected edge")
      val edge = DiCorridorEdge(v3, v4)
      result.asGraph should be (Graph[RoomVertex, CorridorEdge](edge))

    }

    it ("should remove a directed edge and add an undirected instead") {

      Given("a component production that removed a directed edge and adds an undirected edge using the same vertex pair")
      val f = vertices
      import f._

      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v3~>v4))
      val morphism = new Morphism[RoomVertex, String](Map(pa -> v3, pb -> v4))

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .removeEdge(pa, pb, Directed)
        .addOldEdge(pa, pb)

      When("applying the production to a level with an undirected edge")
      val result = prod.apply(morphism, totalLevel)

      Then("the resulting graph should contain an undirected edge")
      val edge = CorridorEdge(v3, v4)
      result.asGraph should be (Graph(edge))

    }

    it ("should apply all rules in the same production") {

      Given("a graph with 4 vertices and a production that performs every modification")
      val f = vertices
      import f._
      Graph[String, UnDiEdge](pa~pb, pa~pc, pc~pd) // Pattern. Not used, just displayed for readability
      val totalLevel = GraphLevel(Graph[RoomVertex, CorridorEdge](v5~v6, v5~v7, v7~v8))
      val morphism = new Morphism[RoomVertex, String](Map(pa -> v5, pb -> v6, pc -> v7, pd -> v8))
      val vertexToAdd1 = new RoomVertex(9)
      val vertexToAdd2 = new RoomVertex(10)

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
        .addVertex(1) // Adds new vertex
        .addVertex(2)
        .addNewEdge(1, 2) // Adds an edge between two new vertices
        .addMixEdge(2, pa) // Adds an edge between a new and an old vertex
        .addOldEdge(pa, pd) // Adds an edge between two old vertices
        .removeVertex(pb) // Removes v3 and edges v1~v2
        .removeEdge(pa, pc, Undirected) // removes edge between v1 and v3

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
      val morphism = new Morphism[RoomVertex, String](Map())

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
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
      val morphism = new Morphism[RoomVertex, String](Map())

      val prod = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, String]()
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
