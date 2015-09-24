package net.cyndeline.scalarlib.rldungeon.common

/**
 * Extended by vertex-classes, representing rooms in the map graph.
 *
 * IMPORTANT NOTICE TO USERS: This class requires equals() and hashcode() to be overwritten. The reason for this
 * is to allow users to generate specific levels by seeding the random number generator. There are multiple instances
 * in the level building algorithm where every room in the level is iterated over, and the order of these rooms may
 * (or may not) affect the final level. The order of rooms in these iterators depend on their hash values.
 * If these methods were not overwritten, seeding the Random object could still give different results on each build
 * attempt. Because of this, equals() and hashcode() must only depend on the fields of the room, and two rooms with
 * the same fields must always generate the same hash and be considered equal.
 *
 * @constructor Constructs a new room with the minimum amount of fields needed by the level strategy.
 * @param rid a unique id for this room that is shared only by copies derived when modifying the room. This id should
 *           only be used when comparing rooms, do not assume that a particular id will end up being present in
 *           a level.
 */
abstract class Room(val rid: Int) {

  /**
   * @param other Room to compare this room to.
   * @return True if both rooms contain the same data (even if they're different object instances), otherwise false.
   */
  override def equals(other: Any): Boolean

  /**
   * @return An integer hash code based on the same data as equals.
   */
  override def hashCode: Int

}
