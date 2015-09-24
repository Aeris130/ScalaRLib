package net.cyndeline.scalarlib.rldungeon.dgs.strategy.pointlessArea

import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.CollapsedNode
import net.cyndeline.scalarlib.util.UnorderedPair

/**
 * Stores data related to pointless vertices and edges in a level.
 *
 * @param bottlenecks Edges that must be traversed when moving from one area to another (may or may not be on the
 *                    main path).
 * @param activatorsResponders Stores the room/corridor pairs that received a matching activator/responder. Rooms and
 *                             corridors may appear multiple times in this vector.
 * @param rewards All rooms that has been assigned a reward.
 */
class PointlessAreaData private (val bottlenecks: Set[(Int, Int)],
                                 val activatorsResponders: Vector[(Set[Int], (Int, Int))],
                                 val rewards: Set[Int],
                                 val hasActivatorCapacity: Set[Int],
                                 val responderCapacity: Map[UnorderedPair[Int], Int],
                                 val hasRewardCapacity: Set[Int]) {

  def this() = this(Set(), Vector(), Set(), Set(), Map(), Set())

  def isBottleneck(from: Int, to: Int): Boolean = bottlenecks.contains((from, to))
  def canHoldActivator(r: Int): Boolean = hasActivatorCapacity.contains(r)
  def canHoldResponder(from: Int, to: Int): Boolean = {
    val pair = UnorderedPair(from, to)
    if (!responderCapacity.contains(pair))
      false
    else
      responderCapacity(pair) > 0
  }
  def canHoldReward(r: Int): Boolean = hasRewardCapacity(r)
  def reduceResponderCapacity(from: Int, to: Int) = {
    val pair = UnorderedPair(from, to)
    new PointlessAreaData(bottlenecks, activatorsResponders, rewards, hasActivatorCapacity, responderCapacity + (pair -> (responderCapacity(pair) - 1)), hasRewardCapacity)
  }
  def getResponderCapacity(from: Int, to: Int): Int = {
    val pair = UnorderedPair(from, to)
    responderCapacity.get(pair).getOrElse(0)
  }

  def markBottleneck(from: Int, to: Int): PointlessAreaData =             new PointlessAreaData(bottlenecks + ((from, to)), activatorsResponders, rewards, hasActivatorCapacity, responderCapacity, hasRewardCapacity)
  def setResponderCapacity(from: Int, to: Int, c: Int): PointlessAreaData =       new PointlessAreaData(bottlenecks, activatorsResponders, rewards, hasActivatorCapacity, responderCapacity + (UnorderedPair(from, to) -> c), hasRewardCapacity)
  def setActivatorCapacity(room: Int): PointlessAreaData =                new PointlessAreaData(bottlenecks, activatorsResponders, rewards, hasActivatorCapacity + room, responderCapacity, hasRewardCapacity)
  def setRewardCapacity(room: Int): PointlessAreaData =                   new PointlessAreaData(bottlenecks, activatorsResponders, rewards, hasActivatorCapacity + room, responderCapacity, hasRewardCapacity + room)

  def addActivatorAndResponder(rooms: Set[Int], corridor: (Int, Int)): PointlessAreaData = {
    new PointlessAreaData(bottlenecks, (rooms, corridor) +: activatorsResponders, rewards, hasActivatorCapacity, responderCapacity, hasRewardCapacity)
  }

  def addReward(room: Int): PointlessAreaData = {
    new PointlessAreaData(bottlenecks, activatorsResponders, rewards + room, hasActivatorCapacity, responderCapacity, hasRewardCapacity)
  }

  def ++(other: PointlessAreaData): PointlessAreaData = {
    new PointlessAreaData(other.bottlenecks ++ bottlenecks,
      other.activatorsResponders ++ activatorsResponders,
      other.rewards ++ rewards,
      other.hasActivatorCapacity ++ hasActivatorCapacity,
      other.responderCapacity ++ responderCapacity,
      other.hasRewardCapacity ++ hasRewardCapacity)
  }

  override def equals(other: Any): Boolean = other match {
    case a: PointlessAreaData => {
      a.bottlenecks == bottlenecks &&
      a.activatorsResponders == activatorsResponders &&
      a.rewards == rewards &&
      a.hasActivatorCapacity == hasActivatorCapacity &&
      a.responderCapacity == responderCapacity &&
      a.hasRewardCapacity == hasRewardCapacity
    }
  }
}
