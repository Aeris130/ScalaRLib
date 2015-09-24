package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.factory

import net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.representation.{FullCorridor, MutableArea}
import net.cyndeline.scalarlib.rldrawing.util.Point

/**
 * Represents an entire map and every area in it.
 *
 * @constructor Constructs a new area representation.
 * @param rooms Every room area on the map.
 * @param bends Every bend area that exist in any corridor.
 * @param corridors Every corridor on the map.
 * @param roomMap A mapping between the original room objects supplied by the user and the areas they were parsed into.
 * @param corridorMap A mapping between the original edge objects supplied by the user and the corridors they were
 *                    parsed into.
 * @param min The lowest x/y values that an area in this representation may assume.
 * @param max The highest x/y values that an area in this representation may assume.
 */
case class AreaRepresentation[VType, EType](rooms: Set[MutableArea],
                                            bends: Set[MutableArea],
                                            corridors: Set[FullCorridor],
                                            roomMap: Map[VType, MutableArea],
                                            corridorMap: Map[EType, FullCorridor],
                                            min: Point, max: Point) {

  val allAreas: Vector[MutableArea] = (rooms ++ bends ++ corridors.map(c => c.areas).flatten).toVector

}
