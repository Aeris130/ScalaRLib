package net.cyndeline.scalarlib.rldungeon.common

import scalax.collection.GraphEdge.UnDiEdge
import net.cyndeline.scalarlib.rldungeon.levelPath.TreePath

/**
 * Contains methods used to mark pointless areas in a level, and to examine the initial level before processing
 * pointless areas in order to add points of interest to them. While it is up to the user to implement strategies
 * that handles however many start/goal rooms is needed for a level, the pointless area strategies require that a
 * single start/goal is specified.
 *
 * @tparam P Class type implementing this trait.
 * @tparam R Class type representing rooms in the level.
 * @tparam C Class type representing corridors/connections in the level.
 */
trait PointlessLevel[P <: PointlessLevel[P, R, C], R, C[X] <: UnDiEdge[X]] extends Level[P, R, C] {

  /** @return The room where the player enters the level. */
  def start: R

  /** @return The room where the player exists the level. */
  def goal: R

  /**
   * This value will only be used to put an upper bound on the number of responders placed, it is not the responsibility
   * of the implementing class to ensure that the specified amount fits on the level, or that it remains beatable after
   * all responders has been placed. As the number and placement of responders placed depends highly on the level
   * topology itself, there's no strict need for this value to be generated randomly.
   *
   * @return The number of responders that should be placed on the map when processed by the ActivatorResponder
   *         strategy.
   */
  def responderAmount: Int

  /**
   * Marks a set of rooms and a corridor as the recipient of a common activator/responder pair (see the
   * ActivatorResponder strategy class).
   *
   * @param rooms A set of all rooms eligible for receiving the activator. It is up to the user to select which
   *              room to mark.
   * @param connection The connection that should receive the responder.
   * @return A copy of this level with the supplied room and connection having been tagged with a common
   *         activator/responder.
   */
  def addActivatorAndResponder(rooms: Set[R], connection: C[R]): P

  /**
   * Marks rooms as a recipient of a reward.
   * @param rooms Rooms to assign rewards to.
   * @return A copy of this level with the specified rooms marked as receiving a reward (one each).
   */
  def addRewards(rooms: Vector[R]): P

  /**
   * @param room A room in the level.
   * @return The amount of additional activators that the room may be assigned. 0 if no activators can be carried.
   */
  def remainingActivatorCapacity(room: R): Int

  /**
   * @param room A room in the level.
   * @return The amount of additional rewards that the room may be assigned. 0 if no rewards can be carried.
   */
  def remainingRewardCapacity(room: R): Int

  /**
   * @return The number of responders that the input connection can carry initially. 0 if no responders can be carried.
   */
  def remainingResponderCapacity(connection: C[R]): Int

  /**
   * If this level object is sent to the activator/responder algorithm, this method will be called to inform the level
   * in which order the player must traverse the rooms in order to visit every activator and responder before reaching
   * the goal. This information is needed in order to perform future evaluations on level difficulty, and also to
   * modify the areas of the map that isn't covered by the main path.
   *
   * @param path A sequence of room sets, in the order they must be traversed when completing the level. Also contains
   *             the pointless areas of the level. See the TreePath class documentation for more information.
   * @return A copy of this level, updated with the path.
   */
  def markMainPath(path: TreePath): P

}
