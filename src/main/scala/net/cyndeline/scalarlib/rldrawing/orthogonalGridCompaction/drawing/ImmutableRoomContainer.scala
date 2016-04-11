package net.cyndeline.scalarlib.rldrawing.orthogonalGridCompaction.drawing

import net.cyndeline.rlcommon.util.Rectangle
import net.cyndeline.scalarlib.rldrawing.common.DrawnRoom

/**
 * Stores room drawing data.
 *
 * @constructor Constructs a new immutable room container.
 * @param originalRoom The room object this room area was originally based on.
 * @param area Start and stop coordinates for this room when drawn on a 2D grid.
 */
case class ImmutableRoomContainer[RoomType](originalRoom: RoomType,
                                            area: Rectangle
                                            ) extends DrawnRoom[RoomType]
