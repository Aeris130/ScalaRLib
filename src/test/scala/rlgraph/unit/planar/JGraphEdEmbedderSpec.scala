package rlgraph.unit.planar

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.rlgraph.planar.jgraphed.JGraphEdEmbedder
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._
import rlgraph.help.CycleOrderMatcher
import net.cyndeline.scalarlib.rlgraph.face.{Face, FaceComputation}

@RunWith(classOf[JUnitRunner])
class JGraphEdEmbedderSpec extends FunSpec with GivenWhenThen with ShouldMatchers {
  private val embedder = new JGraphEdEmbedder[Int, UnDiEdge]()
  private val cycleMatcher = new CycleOrderMatcher()
  private val faceComp = new FaceComputation[Int]()

  describe("JGraphEdEmbedder") {

    it ("should reject a non-planar graph") {

      Given("a non-planar graph")
      val graph = Graph(1~2, 1~3, 1~4, 1~5, 2~3, 2~4, 2~5, 3~4, 3~5, 4~5)

      When("embedding the graph")
      val embedding = embedder.embed(graph)

      Then("the result should be None")
      embedding should be (None)

    }

    it ("should embed single vertices") {

      Given("a graph with vertices and no edges")
      val graph = Graph[Int, UnDiEdge](1, 2)

      When("embedding the graph")
      val embedding = embedder.embed(graph).get

      Then("the embedding should contain two empty entries for 1 and 2")
      cycleMatcher.compareBothDirections(embedding.embeddedVertices, Vector(1, 2)) should be (true)
      embedding.embeddingFor(1).isEmpty should be (true)
      embedding.embeddingFor(2).isEmpty should be (true)

    }

    it ("should embed edges") {

      Given("a graph with edges forming faces 1, 2, 3 / 2, 4, 3 / 1, 2, 4, 3")
      val graph = Graph(1~2, 1~3, 2~3, 2~4, 3~4)

      When("embedding the graph")
      val embedding = embedder.embed(graph).get

      Then("the embedding should have the faces of the original graph")
      val faces = faceComp.computeFaces(embedding)
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f.vertices, Vector(1, 2, 3))))
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f.vertices, Vector(1, 2, 3))))
      assert(faces.exists(f => cycleMatcher.compareBothDirections(f.vertices, Vector(1, 2, 4, 3))))

    }

  }
}
