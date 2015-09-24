package rldungeon.unit.strategy

import net.cyndeline.scalarlib.rldungeon.dgs.strategy.hillclimbing.modules.Validator
import net.cyndeline.scalarlib.rldungeon.dgs.{Parameter, ParameterEstimator}
import rldungeon.help.{CorridorEdge, GraphLevel, RoomVertex}
import testHelpers.SpecImports

import scalax.collection.immutable.Graph

class ValidatorSpec extends SpecImports {

  def fixture = new {
    val validator = new Validator()

    val estimator1 = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]
    val estimator2 = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]
    val estimator3 = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]

    val param1 = new Parameter("P1", 0, 1, 0.1, estimator1, false)
    val param2 = new Parameter("P2", 0, 1, 0.1, estimator2, false)
    val param3 = new Parameter("P3", 0, 1, 0.1, estimator3, false)

    val defaultEstimates = Map(param1 -> 0.0, param2 -> 0.0, param3 -> 0.0)

  }

  describe("Validator") {

    it ("should estimate a value for every parameter and return it") {

      Given("an input graph")
      val f = fixture
      import f._
      val g = GraphLevel(Graph[RoomVertex, CorridorEdge]())

      When("validating the graph using a parameter set")
      val parameters = Set(param1, param2, param3)

      Then("every parameter should be queried for a new estimate using the graph")
      (estimator1.value _) expects(g) returns(1) once()
      (estimator2.value _) expects(g) returns(1) once()
      (estimator3.value _) expects(g) returns(1) once()

      // Start test
      validator.validateModifiedGraph(parameters, defaultEstimates, g)

    }

    it ("should return every parameter that accepts the new estimate in the set of accepting parameters") {

      Given("two parameters that accept the new graph, and 1 tht doesn't")
      val f = fixture
      import f._
      val g = GraphLevel(Graph[RoomVertex, CorridorEdge]())

      (estimator1.value _) expects(g) returns(2) once()
      (estimator2.value _) expects(g) returns(2) once()
      (estimator3.value _) expects(g) returns(2) once()

      // Parameter 1 and 2 targets the value ((4 - 0) / 2), which 2 is closer to than 0.0
      val param1 = new Parameter("P1", 0, 4, 0.1, estimator1, false)
      val param2 = new Parameter("P2", 0, 4, 0.1, estimator2, false)
      val param3 = new Parameter("P3", 0, 1, 0.1, estimator3, false)

      val initialEstimates = Map(param1 -> 0.0, param2 -> 0.0, param3 -> 0.0)

      When("validating the graph using a parameter set")
      val parameters = Set(param1, param2, param3)
      val result = validator.validateModifiedGraph(parameters, initialEstimates, g)

      Then("the accepting parameter set returned should contain the two accepting parameters")
      result._1 should be (Set(param1, param2))

    }

    it ("should return every parameter that rejects the new estimate in the set of rejecting parameters") {

      Given("two parameters that accept the new graph, and 1 tht doesn't")
      val f = fixture
      import f._
      val g = GraphLevel(Graph[RoomVertex, CorridorEdge]())

      (estimator1.value _) expects(g) returns(2) once()
      (estimator2.value _) expects(g) returns(2) once()
      (estimator3.value _) expects(g) returns(2) once()

      // Parameter 3 targets the value ((1 - 0) / 2), which 2 is further from than 0.0
      val param1 = new Parameter("P1", 0, 4, 0.1, estimator1, false)
      val param2 = new Parameter("P2", 0, 4, 0.1, estimator2, false)
      val param3 = new Parameter("P3", 0, 1, 0.1, estimator3, false)

      val initialEstimates = Map(param1 -> 0.0, param2 -> 0.0, param3 -> 0.0)

      When("validating the graph using a parameter set")
      val parameters = Set(param1, param2, param3)
      val result = validator.validateModifiedGraph(parameters, initialEstimates, g)

      Then("the accepting parameter set returned should contain the two accepting parameters")
      result._2 should be (Set(param3))

    }

    it ("should return a map of all new estimates") {

      Given("an input graph and a set of parameters that values it as 1, 2 and 3")
      val f = fixture
      import f._
      val g = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      (estimator1.value _) expects(g) returns(1) once()
      (estimator2.value _) expects(g) returns(2) once()
      (estimator3.value _) expects(g) returns(3) once()

      When("validating the graph using a parameter set")
      val parameters = Set(param1, param2, param3)
      val result = validator.validateModifiedGraph(parameters, defaultEstimates, g)

      Then("the returned map should map parameters 1, 2 and 3 to the estimates 1, 2 and 3")
      result._3 should be (Map(param1 -> 1, param2 -> 2, param3 -> 3))

    }
  }
}
