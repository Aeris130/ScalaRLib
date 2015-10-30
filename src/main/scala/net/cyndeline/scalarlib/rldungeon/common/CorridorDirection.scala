package net.cyndeline.scalarlib.rldungeon.common

/**
 * Used when specifying which direction (if any) a corridor should point at in a level graph.
 */
trait CorridorDirection

case object Undirected extends CorridorDirection
case object Directed extends CorridorDirection


