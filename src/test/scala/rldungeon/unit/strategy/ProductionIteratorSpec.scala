package rldungeon.unit.strategy

import net.cyndeline.rlcommon.util.RandomCollection
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.ParameterResponderValidation
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.hillclimbing.modules.{ProductionIterator, ValidatorI}
import net.cyndeline.scalarlib.rldungeon.dgs.{Parameter, ParameterEstimator}
import net.cyndeline.scalarlib.rldungeon.grammar.production.LevelProduction
import net.cyndeline.scalarlib.subcut.ProjectConfiguration
import rldungeon.help.{CorridorEdge, GraphLevel, RoomVertex}
import testHelpers.SpecImports

import scalax.collection.immutable.Graph

class ProductionIteratorSpec extends SpecImports {

  ProjectConfiguration.modifyBindings { implicit module =>  // implicit makes the test module default
    import module._

    def fixture = new {
    val estimator1 = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]
    val estimator2 = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]
    val estimator3 = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]

    val param1 = new Parameter("P1", 0, 1, 0.1, estimator1, false)
    val param2 = new Parameter("P2", 0, 1, 0.1, estimator2, false)
    val param3 = new Parameter("P3", 0, 1, 0.1, estimator3, false)

    val production1 = mock[LevelProduction[GraphLevel, RoomVertex, CorridorEdge]]
    val production2 = mock[LevelProduction[GraphLevel, RoomVertex, CorridorEdge]]

    val room1 = new RoomVertex(1)
    val room2 = new RoomVertex(2)

    val productionCollection = mock[RandomCollection[LevelProduction[GraphLevel, RoomVertex, CorridorEdge]]]

    /* There's no need to chain multiple collection mocks using the copy method. Just add any subsequent
     * expectations to this mock.
     */
    (productionCollection.copy _) expects() returns(productionCollection) anyNumberOfTimes()

    val responseValidator = mock[ParameterResponderValidation[GraphLevel, RoomVertex, CorridorEdge]]

    val parameterValidator = mock[ValidatorI]
    bind[ValidatorI] toProvider { parameterValidator }
  }

  describe("ProductionIterator") {

    it ("should remove productions that doesn't yield results") {

      Given("a set of productions that yield None")
      val f = fixture
      import f._
      val g = GraphLevel(Graph[RoomVertex, CorridorEdge]())

      (production1.apply _) expects(*) returns(None) once()
      (production2.apply _) expects(*) returns(None) once()

      When("iterating over the production set")
      setRandomCollectionNotEmpty(productionCollection, 2)
      (productionCollection.next _) expects() returns(production1) once()
      (productionCollection.next _) expects() returns(production2) once()

      Then("the production collection should remove every production in the set")
      (productionCollection.remove _) expects(production1) returns() once()
      (productionCollection.remove _) expects(production2) returns() once()

      // Start test
      val iterator = new ProductionIterator()
      iterator.applyProductions(g, Set(param1), productionCollection, responseValidator)

    }

    it ("should send a yielded result to be evaluated by the parameter set") {

      Given("a set of parameters")
      val f = fixture
      import f._
      val g = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      val parameters = Set(param1, param2)

      // Only one production is found
      (productionCollection.next _) expects() returns(production1) once()
      setRandomCollectionNotEmpty(productionCollection, 1)

      When("a production yields a graph")
      val updatedGraph = GraphLevel(Graph[RoomVertex, CorridorEdge](room1))
      (production1.apply _) expects(g) returns(Option(updatedGraph)) once()

      Then("the validator should receive the entire parameter set and the new graph")
      (parameterValidator.validateModifiedGraph[GraphLevel, RoomVertex, CorridorEdge] _) expects(parameters, *, updatedGraph) returns((Set(), Set(), Map())) once()

      // Not a part of this test
      (responseValidator.levelModificationValidates _) expects(*, *) returns(false) once()
      (productionCollection.remove _) expects(*) returns() anyNumberOfTimes()

      // Start test
      val iterator = new ProductionIterator()
      iterator.applyProductions(g, parameters, productionCollection, responseValidator)

    }

    it ("should use a response validator to determine if the set of parameter responses makes a modification valid") {

      Given("a parameter validator that splits a parameter set into accepting and rejecting parameters")
      val f = fixture
      import f._
      val parameters = Set(param1, param2)
      val acceptingParameters = Set(param1)
      val rejectingParameters = Set(param2)
      (parameterValidator.validateModifiedGraph[GraphLevel, RoomVertex, CorridorEdge] _) expects(*, *, *) returns((acceptingParameters, rejectingParameters, Map())) once()

      // Only one production is found
      (productionCollection.next _) expects() returns(production1) once()
      setRandomCollectionNotEmpty(productionCollection, 1)

      When("a production yields a graph")
      val g = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      (production1.apply _) expects(g) returns(Option(g)) once()

      Then("the response validator should receive the accepting and rejecting parameter set")
      (responseValidator.levelModificationValidates _) expects(acceptingParameters, rejectingParameters) returns(false) once()

      // Not a part of this test
      (productionCollection.remove _) expects(*) returns() anyNumberOfTimes()

      // Start test
      val iterator = new ProductionIterator()
      iterator.applyProductions(g, parameters, productionCollection, responseValidator)

    }

    it ("should reject a modification that is approved by the response validator if a rejecting parameter has priority") {

      Given("a parameter that has priority and a production that produces a graph to be validated by the parameters")
      val f = fixture
      import f._
      val prioParam = new Parameter("Priority Parameter", 0, 1, 0.1, estimator3, true)
      val parameters = Set(param1, prioParam)
      val acceptingParameters = Set(param1)
      val rejectingParameters = Set(prioParam)

      // Only one production is found, and it yields a result
      (productionCollection.next _) expects() returns(production1) once()
      setRandomCollectionNotEmpty(productionCollection, 1)
      val g = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      (production1.apply _) expects(g) returns(Option(g)) once()

      When("the parameter validator sorts the priority parameter into the rejecting set and the response validator approves the result")
      (parameterValidator.validateModifiedGraph[GraphLevel, RoomVertex, CorridorEdge] _) expects(*, *, *) returns((acceptingParameters, rejectingParameters, Map())) once()
      (responseValidator.levelModificationValidates _) expects(acceptingParameters, rejectingParameters) returns(true) once()

      Then("the production collection should still remove the production")
      (productionCollection.remove _) expects(production1) returns() once()

      // Start test
      val iterator = new ProductionIterator()
      iterator.applyProductions(g, parameters, productionCollection, responseValidator)

    }

    it ("should update the current set of estimates if a production result is approved by the parameters") {

      Given("a production that yields a result twice and a parameter validator that returns an updated map of estimates for the parameter set")
      val f = fixture
      import f._
      val parameters = Set(param1, param2)

      // Only one production is found, and it yields a result twice
      (productionCollection.next _) expects() returns(production1) anyNumberOfTimes()
      setRandomCollectionNotEmpty(productionCollection, 2)

      val g = GraphLevel(Graph[RoomVertex, CorridorEdge]())
      (production1.apply _) expects(g) returns(Option(g)) anyNumberOfTimes()

      When("parameter validation computes estimates, sorts the parameters into sets, and the response validator approves the parameter result")
      (responseValidator.levelModificationValidates _) expects(*, *) returns(true) anyNumberOfTimes()

      val estimates = Map(param1 -> 1.0, param2 -> 3.0) // Content doesn't really matter
      (parameterValidator.validateModifiedGraph[GraphLevel, RoomVertex, CorridorEdge] _) expects(*, *, *) returns((Set(), Set(), estimates)) once()

      Then("the updates estimation should be sent to the parameter validator when evaluating the second modification")
      (parameterValidator.validateModifiedGraph[GraphLevel, RoomVertex, CorridorEdge] _) expects(*, estimates, *) returns((Set(), Set(), Map())) once()

      // Not a part of this test
      (productionCollection.size _) expects() returns(1) anyNumberOfTimes()

      // Start test
      val iterator = new ProductionIterator()
      iterator.applyProductions(g, parameters, productionCollection, responseValidator)
    }

    it ("should return the modified graph if any modifications were accepted") {

      Given("a production that yields a result")
      val f = fixture
      import f._
      val parameters = Set(param1, param2)

      // Only one production is found, and it yields a result once
      (productionCollection.next _) expects() returns(production1) once()
      setRandomCollectionNotEmpty(productionCollection, 1)

      val g = GraphLevel(Graph[RoomVertex, CorridorEdge](room1))
      (production1.apply _) expects(g) returns(Option(g)) once()

      // Not a part of this test
      (productionCollection.size _) expects() returns(1) anyNumberOfTimes()

      When("the parameter validator sorts the priority parameter into the rejecting set and the response validator approves the result")
      (parameterValidator.validateModifiedGraph[GraphLevel, RoomVertex, CorridorEdge] _) expects(*, *, *) returns((Set(), Set(), Map())) once()
      (responseValidator.levelModificationValidates _) expects(*, *) returns(true) once()

      Then("the resulting graph should be the one returned by the production")
      val iterator = new ProductionIterator()
      val resultingGraph = iterator.applyProductions(g, parameters, productionCollection, responseValidator)
      resultingGraph should be (g)

    }

    it ("should return the unmodified graph if no modifications were possible") {

      Given("a production that yield None")
      val f = fixture
      import f._

      (production1.apply _) expects(*) returns(None) once()

      When("no production resturns an accepted modification")
      setRandomCollectionNotEmpty(productionCollection, 1)
      (productionCollection.next _) expects() returns(production1) once()
      (productionCollection.remove _) expects(production1) returns() once()

      Then("the returned graph should be the same as the input graph")
      val iterator = new ProductionIterator()
      val g = GraphLevel(Graph[RoomVertex, CorridorEdge](room1))
      val resultingGraph = iterator.applyProductions(g, Set(param1), productionCollection, responseValidator)
      resultingGraph should be (g)

    }

    it ("should return the unmodified graph if a modification were possible but not approved by the parameters") {

      Given("a production that yields a result")
      val f = fixture
      import f._
      val parameters = Set(param1, param2)

      // Only one production is found, and it yields a result once
      (productionCollection.next _) expects() returns(production1) once()
      setRandomCollectionNotEmpty(productionCollection, 1)

      val producedGraph = GraphLevel(Graph[RoomVertex, CorridorEdge](room1))
      (production1.apply _) expects(*) returns(Option(producedGraph)) once()

      // Not a part of this test
      (productionCollection.size _) expects() returns(1) anyNumberOfTimes()
      (productionCollection.remove _) expects(*) returns() once()

      When("the parameter validator sorts the priority parameter into the rejecting set and the response validator rejects the result")
      (parameterValidator.validateModifiedGraph[GraphLevel, RoomVertex, CorridorEdge] _) expects(*, *, *) returns((Set(), Set(), Map())) once()
      (responseValidator.levelModificationValidates _) expects(*, *) returns(false) once()

      Then("the returned graph should be the same as the input graph")
      val iterator = new ProductionIterator()
      val inputGraph = GraphLevel(Graph[RoomVertex, CorridorEdge](room2))
      val resultingGraph = iterator.applyProductions(inputGraph, parameters, productionCollection, responseValidator)
      resultingGraph should be (inputGraph)

    }
  }
  } // DI

  private def setRandomCollectionNotEmpty(collectionMock: RandomCollection[LevelProduction[GraphLevel, RoomVertex, CorridorEdge]],
                                          timesIsEmptyReturnsFalse: Int) {
    for(i <- 0 until timesIsEmptyReturnsFalse) {
      (collectionMock.isEmpty _) expects() returns(false)
    }

    (collectionMock.isEmpty _) expects() returns(true)
  }
}
