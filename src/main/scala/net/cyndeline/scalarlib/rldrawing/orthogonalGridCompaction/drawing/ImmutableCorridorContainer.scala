package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.drawing

import net.cyndeline.rlcommon.util.RectangleCoordinates
import net.cyndeline.scalarlib.rldrawing.util.Connection

/**
 * Stores corridor drawing data.
 *
 * @constructor Constructs a new immutable corridor container.
 * @param start One of the original rooms connected by the edge this corridor represents.
 * @param stop The other original room connected by the edge this corridor represents.
 * @param startConnection A range of coordinates where the start room intersects the first corridor segment.
 * @param stopConnection A range of coordinates where the stop room intersects the last corridor segment.
 * @param corridorSegments A list of rectangular areas making up the actual corridor. If the corridor lack bends, this
 *                         list contains a single entry. Otherwise it consists of corridor segments alternated with
 *                         square bend segments.
 * @param corridorConnections If the corridor contains bends, this list contains connections between corridor segments
 *                            and bends in the order they appear in the segment list.
 * @param originalCorridor The original edge that this corridor was based on.
 */
case class ImmutableCorridorContainer[RoomType, CorridorType](start: RoomType,
                                                              stop: RoomType,
                                                              startConnection: Connection,
                                                              stopConnection: Connection,
                                                              corridorSegments: Vector[RectangleCoordinates],
                                                              corridorConnections: Vector[Connection],
                                                              originalCorridor: CorridorType
                                                              ) extends DrawnCorridor[RoomType, CorridorType]
