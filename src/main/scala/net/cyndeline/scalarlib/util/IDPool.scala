package net.cyndeline.scalarlib.util

import scala.collection.immutable.Queue

/**
 * Immutable ID factory that produces integer ID's and allows the user to free up ID's, causing them to be reused
 * before additional ID's are assigned.
 */
class IDPool private (nextAvailableId: Int, currentPool: Queue[Int]) {

  /**
   * Constructs an empty ID pool beginning at ID 0, with no reusable IDs in the pool.
   */
  def this() = this(0, Queue())

  /**
   * Constructs an empty ID pool beginning at a specified ID, with no reusable ID's in the pool.
   * @param startingId The first ID to assign using this pool. The next ID returned will be this one + 1 and so on.
   */
  def this(startingId: Int) = this(startingId, Queue())

  /** @return The next available ID, along with an updated ID pool. */
  def nextId: (Int, IDPool) = {
    if (currentPool.isEmpty) {
      if (nextAvailableId >= Int.MaxValue)
        throw new Error("Maximum ID count reached in the pool: " + Int.MaxValue)

      (nextAvailableId, new IDPool(nextAvailableId + 1, currentPool))
    } else {
      val poolEntry = currentPool.dequeue
      (poolEntry._1, new IDPool(nextAvailableId, poolEntry._2))
    }
  }

  /**
   * Marks an ID as available for reuse.
   * @param id ID to reuse.
   * @return An ID pool with the specified ID marked for reuse.
   */
  def deleteID(id: Int): IDPool = new IDPool(nextAvailableId, currentPool.enqueue(id))

  /**
   * Marks an ID as available for reuse.
   * @param id ID to reuse.
   * @return An ID pool with the specified ID marked for reuse.
   */
  def -(id: Int): IDPool = deleteID(id)

}
