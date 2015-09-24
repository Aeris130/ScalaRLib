package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation


/**
 * Stores corridor areas and their bends in the order visited when traversing the corridor from one room
 * to the other.
 *
 * @constructor Creates a corridor storage.
 * @param from The room connected to the first area in the corridor.
 * @param to The room connected to the last area in the corridor. If the corridor only contains a single segment,
 *           this room is connected to it.
 * @param areas Corridors and bends. If the corridor has no bends, this list contains a single entry. Otherwise
 *              the areas will alternate between corridors and bends (the first and last area will be corridor
 *              segments connecting to rooms).
 */
class FullCorridor(val from: MutableArea, val to: MutableArea, val areas: Vector[MutableArea]) {

  /**
   * @return Iterator over every segment in the corridor (rooms not included).
   */
  def toIterator: Iterator[MutableArea] = areas.toIterator

  override def toString: String = "FullCorridor[" + areas.mkString(", ") + "]"

  override def equals(other: Any): Boolean = other match {
    case fc: FullCorridor => fc.from == from && fc.to == to && fc.areas == areas
    case _ => false
  }

  override def hashCode: Int = from.## ^ to.## ^ areas.##
}
