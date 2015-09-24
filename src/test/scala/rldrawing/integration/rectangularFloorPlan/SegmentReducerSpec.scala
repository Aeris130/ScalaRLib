package rldrawing.integration.rectangularFloorPlan

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import rldrawing.help.ConstraintRoom
import net.cyndeline.scalarlib.rldrawing.util.Geom
import net.cyndeline.scalarlib.rldrawing.util.Direction._
import net.cyndeline.scalarlib.rlgraph.planarGraphDrawing.rectangular.RectangularLayout
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.FloorPlan
import net.cyndeline.scalarlib.rldrawing.rectangularFloorPlan.help.SegmentReducer

@RunWith(classOf[JUnitRunner])
class SegmentReducerSpec extends FunSpec with GivenWhenThen with ShouldMatchers {
  private val defaultRoomAspect = 9999
  private val defaultGateAspect = 9999
  private val defaultRoomSize = 1
  private val defaultGateSize = 1
  private val sReducer = new SegmentReducer(defaultRoomAspect, defaultGateAspect, defaultRoomSize, defaultGateSize)

  def samplePlan = new {
    val defaultStartC = 0

    val room1 = ConstraintRoom(8, 4) // Same target as the actual size, doesn't need to be reduced
    val room2 = ConstraintRoom(3, 3)
    val room3 = ConstraintRoom(3, 3)

    // Upper left room, 8x4
    val r1Coordinates = Geom.areaCoordinates(defaultStartC, 8, 4)
    val r1StopY = Geom.furthestCoordinate(South, r1Coordinates._1, r1Coordinates._2)
    val r1StopX = Geom.furthestCoordinate(East, r1Coordinates._1, r1Coordinates._2)

    // Gate below room1 and above room 2, 8x4
    val gate_1_2_Coordinates = Geom.areaCoordinates(defaultStartC, r1StopY, 8, 4)
    val gateStopY = Geom.furthestCoordinate(South, gate_1_2_Coordinates._1, gate_1_2_Coordinates._2)

    // Room below gate, rightmost part lies below room 3
    val r2Coordinates = Geom.areaCoordinates(defaultStartC, gateStopY, 11, 6)

    // Room to the left of room 1 and the gate, above room 2. Only has height 7 as the two areas to its left
    // (room1 and the gate) shares their middle coordinate, only taking up 7 coordinates.
    val r3Coordinates = Geom.areaCoordinates(r1StopX, defaultStartC, 4, 7)

    val roomCoordinates = Vector(
      (room1, r1Coordinates._1.asTuple, r1Coordinates._2.asTuple),
      (room2, r2Coordinates._1.asTuple, r2Coordinates._2.asTuple),
      (room3, r3Coordinates._1.asTuple, r3Coordinates._2.asTuple)
    )
    val gates = Vector(((room1, room2), gate_1_2_Coordinates._1.asTuple, gate_1_2_Coordinates._2.asTuple))
    val graph = Graph(room1~room2, room1~room3, room2~room3)

    val layout = rectLayoutWithGates(roomCoordinates:_*)(gates:_*)(graph)
    val floorPlan = FloorPlan(layout)
  }

  describe("SegmentReducer") {

    it ("should attempt to minimize areas above their target") {

      Given("a floor plan with rooms 2 and 3 having sizes above their targets")
      val f = samplePlan
      import f._

      When("reducing the plans areas")
      val reducedPlan = sReducer.reduceFloorPlan(floorPlan)

      Then("room 1 should have the same coordinates")
      val originalRoom1 = findRoom(room1, floorPlan)
      val newRoom1 = findRoom(room1, reducedPlan)
      newRoom1.start should be (originalRoom1.start)
      newRoom1.stop should be (originalRoom1.stop)

      And("room 2 should have width 5 to account for the 2-coordinate adjacency margin")
      val newRoom2 = findRoom(room2, reducedPlan)
      Geom.width(newRoom2) should be (5)

      And("room 2 should be smaller than before")
      val originalRoom2 = findRoom(room2, floorPlan)
      assert(Geom.area(newRoom2) < Geom.area(originalRoom2))

      And("room 3 should be smaller than before")
      val newRoom3 = findRoom(room3, reducedPlan)
      assert(Geom.area(newRoom3) < (4*7))

      And("the gate should be smaller than before")
      val gate = reducedPlan.roomAreas.find(_.isGate).get
      assert(Geom.area(gate) < (8*4))

    }

    it ("should not reduce the outer rooms edges past the outermost coordinate") {

      Given("a floor plan with rooms 1 and 3 sharing the same north coordinate, and room1 already having its target size")
      val f = samplePlan
      import f._

      When("reducing the plans areas using room 3 as its entry room")
      val reducedPlan = sReducer.reduceFloorPlanWithEntry(floorPlan, room3)

      Then("room 3 should not have its northern side moved inwards despite needing that to achieve its target size")
      val oldRoom3Area = findRoom(room3, floorPlan)
      val room3Area = findRoom(room3, reducedPlan)
      room3Area.start.y should be (defaultStartC)

      And("room 3 should have its western side moved inwards")
      assert(room3Area.stop.x < oldRoom3Area.stop.x)

    }

    it ("should not reduce rooms below the maximum aspect ratio") {

      Given("a floor plan with room 2 having target size 9 and the smallest side being 6 coordinates long")
      val f = samplePlan
      import f._

      When("reducing the plans areas using a maximum room ratio of 1")
      val ratioReducer = new SegmentReducer(1, defaultGateAspect, defaultRoomSize, defaultGateSize)
      val reducedPlan = ratioReducer.reduceFloorPlan(floorPlan)

      // Room 2 reduces down to 3x3 normally, but can't since it hits 4x4 first with a aspect ratio of 1:1
      Then("neither side of the room should decrease when both sides hit length 6")
      val newRoom2 = findRoom(room2, reducedPlan)
      Geom.width(newRoom2.start, newRoom2.stop) should be (6)
      Geom.height(newRoom2.start, newRoom2.stop) should be (6)

    }

    it ("should not reduce gates below the maximum aspect ratio") {

      Given("a floor plan with a gate that normally can be reduced to size 3x4")
      val f = samplePlan
      import f._

      When("reducing the plans areas using a maximum gate ratio of 1")
      val ratioReducer = new SegmentReducer(defaultRoomAspect, 1, defaultRoomSize, defaultGateSize)
      val reducedPlan = ratioReducer.reduceFloorPlan(floorPlan)

      Then("both sides of the gate should be 4 coordinates long")
      val gate = reducedPlan.roomAreas.find(_.isGate).get
      Geom.width(gate.start, gate.stop) should be (4)
      Geom.height(gate.start, gate.stop) should be (4)

    }

  }

  private def findRoom(r: ConstraintRoom, floorPlan: FloorPlan[ConstraintRoom, UnDiEdge]) = floorPlan.roomAreas.find(_.originalRoom == r).get

  private def rectLayoutWithGates(rooms: (ConstraintRoom, (Int, Int), (Int, Int))*)(gates: ((ConstraintRoom, ConstraintRoom), (Int, Int), (Int, Int))*)(graph: Graph[ConstraintRoom, UnDiEdge]): RectangularLayout[ConstraintRoom, UnDiEdge]
  = new RectangularLayout(rooms.toVector, gates.toVector, Vector(), graph)

}
