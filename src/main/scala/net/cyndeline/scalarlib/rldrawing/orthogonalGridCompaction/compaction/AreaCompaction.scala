package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.compaction

import com.escalatesoft.subcut.inject.{BindingModule, Injectable}
import net.cyndeline.rlcommon.math.geom.Point
import net.cyndeline.rlcommon.util.Direction._
import net.cyndeline.rlcommon.util.{RandomElementFinder, RandomElementFinderInterface}
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.compaction.help.MovementMarker
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.drawing.{GridDrawing, RepresentationToDrawingConverter, RepresentationToDrawingConverterI}
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.MutableArea
import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory.AreaRepresentation

import scala.collection.mutable
import scala.util.Random

/**
 * Compacts a set of areas, one step at the time, and can produce output based on the current compaction at any step.
 *
 * The basic algorithm is to select a single area (either a room or a corridor bend) and attempt to move it one
 * coordinate towards the target along both axises. Any other room, bend or corridor segment that must move in order
 * for this to happen is moved as well. If the selected area cannot move due to being blocked by an area that has
 * already reached the target (or if that has happened for any other area required to move along with it), then the
 * area is placed at the back of the queue.
 *
 * The algorithm terminates when a room fails to move in this manner along with every room before it.
 *
 * @constructor Creates a compaction algorithm with a custom target coordinate finder.
 * @param areas Mutable set of areas that will be compacted every time this algorithm has its compaction method called.
 * @param random Random object used to decide which area should be used as target to compact towards.
 */
class AreaCompaction[VType, EType](areas: AreaRepresentation[VType, EType], random: Random)
                                  (implicit val bindingModule: BindingModule)
  extends Injectable {
  if (areas.rooms.isEmpty || areas.roomMap.isEmpty)
    throw new IllegalArgumentException("Cannot compact an area without rooms.")

  private val drawingConverter = injectOptional[RepresentationToDrawingConverterI].getOrElse {
    new RepresentationToDrawingConverter()
  }
  private val target: Point = findTarget
  private val movementMarker = new MovementMarker(target, areas.min, areas.max)

  /* Keeps track of which area should be moved on the next compaction call. Moved areas are dequeued
   * and enqueued anew.
   */
  private val areaQueue = enqueueAreas
  private var unmovable: Option[MutableArea] = None
  private var noMoreMovementAllowed = false

  /**
   * Can be called at any point between the compaction calls to produce a drawn copy of the areas as they currently
   * are compacted.
   * @return An immutable drawing of the current area set.
   */
  def produceCompactedDrawing: GridDrawing[VType, EType] = drawingConverter.convertToDrawing(areas)

  /**
   * Repeats the compaction process until the map cannot be compacted any further.
   * @return The number of successful compactions performed.
   */
  def completeCompact(): Int = {
    var i = 0
    while (!compact()) {
      i += 1
    }
    i
  }

  /**
   * Moves one area one step along both axises towards the target. If moving one area requires other areas to move
   * with it, those areas will be moved as well.
   * @return True if no areas could be moved (algorithm is finished), otherwise false.
   */
  def compact(): Boolean = {
    if (noMoreMovementAllowed) return false

    while (true) {
      val areaToMove = areaQueue.dequeue()
      areaQueue.enqueue(areaToMove)
      val movementDirections = findDirections(areaToMove)

      val areaCouldMoveInDir1 = if (movementDirections._1.isDefined)
        moveAreaInDirection(areaToMove, movementDirections._1.get)
      else false

      val areaCouldMoveInDir2 = if (movementDirections._2.isDefined)
        moveAreaInDirection(areaToMove, movementDirections._2.get)
      else false

      if (!areaCouldMoveInDir1 && !areaCouldMoveInDir2 && unmovable.isEmpty) {
        unmovable = Option(areaToMove)

      } else if (!areaCouldMoveInDir1 && !areaCouldMoveInDir2 && unmovable.isDefined) {

        /* Every area has been attempted to move, but couldn't, and not we're at the initial area that couldn't
         * be moved. This means no more areas can move.
         */
        if (areaToMove == unmovable.get) {
          noMoreMovementAllowed = true
          return noMoreMovementAllowed
        }

      } else {
        unmovable = None
        return false // An area moved, so there may be additional compactions possible.
      }
    }

    true // Can't happen
  }

  /**
   * Starts the movement process for a single area in a single direction.
   * @return true if the area could be moved, otherwise false.
   */
  private def moveAreaInDirection(area: MutableArea, direction: Direction): Boolean = {
    val movementOrder = new mutable.Stack[Set[MutableArea]]
    var canMove = true
    var done = false

    movementOrder.push(Set(area))
    while (canMove && !done) {
      val currentAreaSetToMove = movementOrder.head

      // False if one of the areas has reached the target (or the grid edge) and refuses to move
      val markingResult = movementMarker.markAreasForMovement(currentAreaSetToMove, direction)

      if (!markingResult) {
        canMove = false // Reset data and abort
      } else {
        val adjacentAreasThatMustMove = findAreasThatMustMove(currentAreaSetToMove, direction)

        // If no more areas must be moved in order for the areas examined up until now to move, we're done
        if (!adjacentAreasThatMustMove.isEmpty) {
          movementOrder.push(adjacentAreasThatMustMove)
        } else {
          done = true
        }
      }
    }

    if (!canMove) {
      resetMovementData(movementOrder)
      false
    } else {
      val movedAreas = moveAreas(movementOrder)
      resetMovementData(movedAreas)
      true
    }
  }

  /**
   * Determines which two directions an area must be moved in to intersect the target coordinate. If an area already
   * intersects a targets coordinate on a single axis, no direction for that axis is returned.
   * @return West | East and North | South.
   */
  private def findDirections(area: MutableArea): (Option[Direction], Option[Direction]) = {
    val xRange = area.area.coordinatesOnSide(North)
    val yRange = area.area.coordinatesOnSide(West)

    val xAxisMovement = if (target.x < xRange._1) Option(West)
    else if (target.x > xRange._2) Option(East)
    else None

    val yAxisMovement = if (target.y < yRange._1) Option(North)
    else if (target.y > yRange._2) Option(South)
    else None

    (xAxisMovement, yAxisMovement)
  }

  /**
   * Moves every area in its intended direction.
   */
  private def moveAreas(stack: mutable.Stack[Set[MutableArea]]): mutable.Stack[Set[MutableArea]] = {
    val movedAreas = mutable.Stack[Set[MutableArea]]()

    while (!stack.isEmpty) {
      val areas = stack.pop()
      areas.foreach(a => a.move())
      movedAreas push areas
    }

    movedAreas
  }

  private def resetMovementData(stack: mutable.Stack[Set[MutableArea]]) {
    while (!stack.isEmpty) {
      for {
        area <- stack.pop()
        if area.movement.isDefined
      } area.clearMovement()
    }
  }

  /**
   * Checks with every area in a set which other areas must move in order for that area to move one step in a specified
   * direction. The result for every area is unioned into a single set.
   */
  private def findAreasThatMustMove(areas: Set[MutableArea], direction: Direction): Set[MutableArea] = {
    val result = (for
    { area <- areas
    } yield area.canMove(direction)).flatten
    result
  }

  /**
   * Selects a random coordinate somewhere within the lowest and highest x/y coordinates in the drawing to minimize
   * the number of movement calls needed before the stop qualifier is reached (an area intersects the target).
   */
  private def findTarget: Point = {
    val targetFinder = injectOptional[TargetFinder] getOrElse { new RandomTargetFinder(random) }
    val randomAreaSelect = injectOptional[RandomElementFinderInterface] getOrElse { new RandomElementFinder() }
    val areaToUse = randomAreaSelect.findRandomElement(areas.rooms union areas.bends, random)
    targetFinder.findTarget(areaToUse.area) // Any area will do
  }

  private def enqueueAreas: mutable.Queue[MutableArea] = {
    val q = mutable.Queue[MutableArea]()
    for (area <- areas.rooms union areas.bends)
      q.enqueue(area)

    q
  }

}
