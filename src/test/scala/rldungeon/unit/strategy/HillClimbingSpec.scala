package rldungeon.unit.strategy

import net.cyndeline.scalarlib.rldungeon.dgs.strategy.ParameterResponderValidation
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.hillclimbing.HillClimbing
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.hillclimbing.modules.ProductionIteratorI
import net.cyndeline.scalarlib.rldungeon.dgs.{Parameter, ParameterEstimator}
import net.cyndeline.scalarlib.subcut.ProjectConfiguration
import rldungeon.help.{CorridorEdge, GraphLevel, RoomVertex}
import testHelpers.SpecImports

import scala.reflect._
import scala.reflect.runtime.universe._
import scalax.collection.immutable.Graph

class HillClimbingSpec extends SpecImports {

  ProjectConfiguration.modifyBindings { implicit module =>  // implicit makes the test module default
    import module._

  def fixture = new {
    val estimator1 = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]
    val estimator2 = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]
    val estimator3 = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]

    val param1 = new Parameter("P1", 0, 2, 0.1, estimator1, false)
    val param2 = new Parameter("P2", 0, 2, 0.1, estimator2, false)
    val param3 = new Parameter("P3", 0, 2, 0.1, estimator3, false)

    val productionIteratorMock = mock[ProductionIteratorI]

    implicit val ct = classTag[ProductionIteratorI]
    implicit val tt = typeTag[ProductionIteratorI]
    bind[ProductionIteratorI] toProvider { productionIteratorMock }

    val responseValidator = mock[ParameterResponderValidation[GraphLevel, RoomVertex, CorridorEdge]]

    val room1 = new RoomVertex(1)
    val room2 = new RoomVertex(2)

    val initialGraph = GraphLevel(Graph[RoomVertex, CorridorEdge]())

  }

  describe("HillClimbing") {

    it ("should feed the initial graph to the production iterator") {

      Given("an input graph")
      val f = fixture
      import f._
      val graph = GraphLevel(Graph[RoomVertex, CorridorEdge](room1))

      When("applying the hill climbing algorithm to the graph")
      Then("the production iterator should receive the graph")
      productionIteratorMock.applyProductions[GraphLevel, RoomVertex, CorridorEdge, String] _ expects(graph, *, *, *) returns graph once()

      // Not a part of this test
      responseValidator.levelModificationValidates _ expects(*, *) returns true anyNumberOfTimes()

      // Start the test
      val hillClimber = new HillClimbing[GraphLevel, RoomVertex, CorridorEdge, String](Set(), null, responseValidator, 1)
      hillClimber.apply(graph)

    }

    it ("should validate the produced graph using every parameter") {

      Given("a production iterator that produces a graph")
      val f = fixture
      import f._
      val graph = GraphLevel(Graph[RoomVertex, CorridorEdge](room1))
      productionIteratorMock.applyProductions[GraphLevel, RoomVertex, CorridorEdge, String] _ expects(*, *, *, *) returns graph once()

      When("modifying a graph with parameters 1, 2 and 3")
      val parameters = Set(param1, param2, param3)
      val hillClimber = new HillClimbing[GraphLevel, RoomVertex, CorridorEdge, String](parameters, null, responseValidator, 1)

      Then("every algorithm parameter should evaluate the resulting graph")
      estimator1.value _ expects graph returns 1 once()
      estimator2.value _ expects graph returns 1 once()
      estimator3.value _ expects graph returns 1 once()

      // Not a part of this test
      responseValidator.levelModificationValidates _ expects(*, *) returns true anyNumberOfTimes()

      // Start test
      hillClimber.apply(initialGraph)

    }

    it ("should sort parameters into accepting/rejecting sets based on the final graph") {

      Given("a production iterator that produces a graph")
      val f = fixture
      import f._
      val graph = GraphLevel(Graph[RoomVertex, CorridorEdge](room1))
      productionIteratorMock.applyProductions[GraphLevel, RoomVertex, CorridorEdge, String] _ expects(*, *, *, *) returns graph once()

      When("applying the hill climbing algorithm to the graph with 2 parameters that approve the result, 1 that rejects")
      val parameters = Set(param1, param2, param3)

      // All 4 parameters have a target between 0 and 2
      estimator1.value _ expects * returns 1
      estimator2.value _ expects * returns 1
      estimator3.value _ expects * returns 99 // Out of bounds = reject

      Then("the parameter validator should receive parameters 1 and 2 as accepting, and 3 as rejecting")
      responseValidator.levelModificationValidates _ expects(Set(param1, param2), Set(param3)) returns true once()

      // Start the test
      val hillClimber = new HillClimbing[GraphLevel, RoomVertex, CorridorEdge, String](parameters, null, responseValidator, 1)
      hillClimber.apply(graph)

    }

    it ("should return a graph that validates") {

      Given("a production iterator that produces a graph")
      val f = fixture
      import f._
      val graph = GraphLevel(Graph[RoomVertex, CorridorEdge](room1))
      productionIteratorMock.applyProductions[GraphLevel, RoomVertex, CorridorEdge, String] _ expects(*, *, *, *) returns graph once()

      When("the response validator accepts the graph")
      responseValidator.levelModificationValidates _ expects(*, *) returns true once()

      Then("the returned graph should be the one produces by the production iterator")

      // The parameter list is Nil, so no estimator mocks has to be set
      val hillClimber = new HillClimbing[GraphLevel, RoomVertex, CorridorEdge, String](Set(), null, responseValidator, 1)
      val result = hillClimber.apply(initialGraph)

      result should be (Option(graph))

    }

    it ("should return None if no graph validates") {

      Given("a production iterator that produces a graph")
      val f = fixture
      import f._
      val graph = GraphLevel(Graph[RoomVertex, CorridorEdge](room1))
      productionIteratorMock.applyProductions[GraphLevel, RoomVertex, CorridorEdge, String] _ expects(*, *, *, *) returns graph once()

      When("the response validator rejects the graph")
      responseValidator.levelModificationValidates _ expects(*, *) returns false once()

      Then("the returned graph should be None")

      // The parameter list is Nil, so no estimator mocks has to be set
      val hillClimber = new HillClimbing[GraphLevel, RoomVertex, CorridorEdge, String](Set(), null, responseValidator, 1)
      val result = hillClimber.apply(initialGraph)

      result should be (None)

    }

    it ("should restart the algorithm if the final map isn't valid according to the specified number of times") {

      Given("a production iterator that produces a graph")
      val f = fixture
      import f._
      val graph1 = GraphLevel(Graph[RoomVertex, CorridorEdge](room1))
      val graph2 = GraphLevel(Graph[RoomVertex, CorridorEdge](room2))
      productionIteratorMock.applyProductions[GraphLevel, RoomVertex, CorridorEdge, String] _ expects(initialGraph, *, *, *) returns graph1 once()

      When("the response validator rejects the first graph")
      responseValidator.levelModificationValidates _ expects(*, *) returns false once()

      Then("the iterator should receive another query")
      productionIteratorMock.applyProductions[GraphLevel, RoomVertex, CorridorEdge, String] _ expects(initialGraph, *, *, *) returns graph2 once()

      // Not a part of the test
      responseValidator.levelModificationValidates _ expects(*, *) returns false once() // Needed during the second run, but not a part of the test

      val hillClimber = new HillClimbing[GraphLevel, RoomVertex, CorridorEdge, String](Set(), null, responseValidator, 2) // <- 2 runs
      hillClimber.apply(initialGraph)

    }

  }
  } // DI
}
