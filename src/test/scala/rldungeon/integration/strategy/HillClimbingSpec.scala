package rldungeon.integration.strategy

import net.cyndeline.rlcommon.util.ProbabilityCollection
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.ParameterAcceptRatio
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.hillclimbing.HillClimbing
import net.cyndeline.scalarlib.rldungeon.dgs.{Parameter, ParameterEstimator}
import net.cyndeline.scalarlib.rldungeon.grammar.production.{LevelProduction, SingleProduction}
import net.cyndeline.scalarlib.rldungeon.grammar.util.{GraphMatcher, TopologyProduction}
import net.cyndeline.scalarlib.subcut.ProjectConfiguration
import rldungeon.help._
import rldungeon.help.parameters.RoomAmountEstimator
import testHelpers.SpecImports

import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

class HillClimbingSpec extends SpecImports {

  ProjectConfiguration.modifyBindings { implicit module =>  // implicit makes the test module default

    def fixture = new {
      val edgeCopyFactory = new CorridorCopyFactory[RoomVertex]()
      val edgeFactory = new CorridorFactory[RoomVertex]()
      val topologyProduction = new TopologyProduction[GraphLevel, RoomVertex, CorridorEdge, Int]()

      val acceptsAllParameters = new ParameterAcceptRatio[GraphLevel, RoomVertex, CorridorEdge](0.00001) // Accepts anything, don't use with conflicting parameters
      val roomAmountEstimator = new RoomAmountEstimator()
      val random = new Random()

      val dummyRoom = new RoomVertex(-99)
      val pattern = Graph[Int, UnDiEdge](-99)// Patterns can't be empty
      val defaultMatcher = GraphMatcher.empty[RoomVertex, CorridorEdge, Int, UnDiEdge]()
    }

  describe("HillClimbing") {

    it ("should apply a production until a parameter is satisfied") {

      Given("a parameter that requests 4 to 7 rooms and a production that adds 1 room")
      val f = fixture
      import f._
      val parameter = new Parameter[GraphLevel, RoomVertex, CorridorEdge]("Room amount", 4, 7, roomAmountEstimator, false)
      val production_right = topologyProduction.addVertex(1)

      val finalProduction = SingleProduction[GraphLevel, RoomVertex, CorridorEdge, Int, UnDiEdge](pattern, defaultMatcher, production_right, random)
      val productionCollection = ProbabilityCollection.from[LevelProduction[GraphLevel, RoomVertex, CorridorEdge, Int]]((1, finalProduction))

      When("applying the hill climber onto a graph using the room parameter")
      val parameters = Set[Parameter[GraphLevel, RoomVertex, CorridorEdge]](parameter)
      val hillClimber = new HillClimbing(parameters, productionCollection, acceptsAllParameters, 1, random)
      val level = GraphLevel(Graph[RoomVertex, CorridorEdge](dummyRoom))
      val resultingLevel = hillClimber.apply(level)

      Then("there should be a result")
      resultingLevel should be ('defined)

      And("the graph should be assigned 4 to 7 rooms")
      val roomAmount = resultingLevel.get.asGraph.nodes.size
      assert(roomAmount >= 4, "Less than 4 rooms found (" + roomAmount + ")")
      assert(roomAmount <= 7, "More than 7 rooms found (" + roomAmount + ")")

    }

    it ("should stop when it can't come any closer to a parameter goal even if the goal has not been reached within its margins") {

      Given("a parameter that requests 9 to 10 rooms and a production that adds 7 room, as well as another parameter that accepts the graph unconditionally")
      val f = fixture
      import f._
      val parameter = new Parameter("Room amount", 9, 10, roomAmountEstimator, false)

      /* If the above parameter were the only one, the graph would not be accepted. To ensure that the graph is accepted
       * and that the amount of accepting parameters exceed the rejecting one, two dummy parameters are added that
       * accepts any graph.
       */
      val unconditionalEstimator = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]
      unconditionalEstimator.value _ expects * returns 0 anyNumberOfTimes()
      val unconditionalParameter1 = new Parameter("Room amount unconditional 1", 0, 1, unconditionalEstimator, false)
      val unconditionalParameter2 = new Parameter("Room amount unconditional 2", 0, 1, unconditionalEstimator, false)

      val production_right = topologyProduction
                              .addVertex(1)
                              .addVertex(2)
                              .addVertex(3)
                              .addVertex(4)
                              .addVertex(5)
                              .addVertex(6)
                              .addVertex(7)

      val finalProduction = SingleProduction[GraphLevel, RoomVertex, CorridorEdge, Int, UnDiEdge](pattern, defaultMatcher, production_right, random)
      val productionCollection = ProbabilityCollection.from[LevelProduction[GraphLevel, RoomVertex, CorridorEdge, Int]]((1, finalProduction))

      When("applying the hill climber onto a graph using the room parameter")
      val parameters = Set[Parameter[GraphLevel, RoomVertex, CorridorEdge]](parameter, unconditionalParameter1, unconditionalParameter2)
      val hillClimber = new HillClimbing(parameters, productionCollection, acceptsAllParameters, 1, random)
      val level = GraphLevel(Graph[RoomVertex, CorridorEdge](dummyRoom))
      val resultingLevel = hillClimber.apply(level).get

      Then("the climber should stop at 8 (7 + 1 for the initial dummy) rooms")
      resultingLevel.asGraph.nodes.size should be (8)

    }

    it ("should apply a conflicting production if the amount of approving parameters outnumber the rejecting ones by a set percentage") {
      // This test is performed above
    }

    it ("should not apply a production when the amount of approving parameters is less than the specified percentage") {

      Given("a parameter that requests 2 to 3 rooms and a production that adds 1 room, as well as another parameter that requests 1 room")
      val f = fixture
      import f._
      val parameter1 = new Parameter("Room amount", 2, 3, roomAmountEstimator, false)
      val parameter2 = new Parameter("Room amount", 1, 1, roomAmountEstimator, false)

      val production_right = topologyProduction.addVertex(1)

      val finalProduction = SingleProduction[GraphLevel, RoomVertex, CorridorEdge, Int, UnDiEdge](pattern, defaultMatcher, production_right, random)
      val productionCollection = ProbabilityCollection.from[LevelProduction[GraphLevel, RoomVertex, CorridorEdge, Int]]((1, finalProduction))

      When("applying the hill climber onto a graph using the room parameter")
      val parameters = Set[Parameter[GraphLevel, RoomVertex, CorridorEdge]](parameter1, parameter2)
      val hillClimber = new HillClimbing(parameters, productionCollection, acceptsAllParameters, 1, random)
      val level = GraphLevel(Graph[RoomVertex, CorridorEdge](dummyRoom))
      val resultingLevel = hillClimber.apply(level)

      Then("the result should be None")
      resultingLevel should be (None)

    }

    it ("should not apply a production that gets rejected by a priority parameter") {
      val f = fixture
      import f._

      Given("a set of 2 parameters that accept a production unconditionally, and a priority parameter that rejects a production unconditionally")
      val unconditionalEstimator1 = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]
      unconditionalEstimator1.value _ expects * returns 0 anyNumberOfTimes()
      val acceptsUnconditionalParameter1 = new Parameter("Accepts room amount unconditional 1", 0, 1, unconditionalEstimator1, false)
      val acceptsUnconditionalParameter2 = new Parameter("Accepts room amount unconditional 2", 0, 1, unconditionalEstimator1, false)

      val unconditionalEstimator2 = mock[ParameterEstimator[GraphLevel, RoomVertex, CorridorEdge]]
      unconditionalEstimator2.value _ expects * returns 99 anyNumberOfTimes()
      val rejectsUnconditionalParameter = new Parameter("Rejects room amount unconditional 1", 0, 1, unconditionalEstimator2, true)

      val production_right = topologyProduction.addVertex(1)

      val finalProduction = SingleProduction[GraphLevel, RoomVertex, CorridorEdge, Int, UnDiEdge](pattern, defaultMatcher, production_right, random)
      val productionCollection = ProbabilityCollection.from[LevelProduction[GraphLevel, RoomVertex, CorridorEdge, Int]]((1, finalProduction))

      When("applying the hill climber onto a graph using the room parameter")
      val parameters = Set[Parameter[GraphLevel, RoomVertex, CorridorEdge]](acceptsUnconditionalParameter1, acceptsUnconditionalParameter2, rejectsUnconditionalParameter)
      val hillClimber = new HillClimbing(parameters, productionCollection, acceptsAllParameters, 1, random)
      val level = GraphLevel(Graph[RoomVertex, CorridorEdge](dummyRoom))
      val resultingLevel = hillClimber.apply(level)

      Then("the result should be None")
      resultingLevel should be (None)

    }

  }
  } // DI
}
