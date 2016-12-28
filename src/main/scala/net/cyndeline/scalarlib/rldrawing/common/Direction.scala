package net.cyndeline.scalarlib.rldrawing.common

/**
  * North, South, West, East.
  */
trait Direction {
  def opposite: Direction
}

case object North extends Direction {
  override def opposite: Direction = South
}
case object South extends Direction {
  override def opposite: Direction = North
}
case object West extends Direction {
  override def opposite: Direction = East
}
case object East extends Direction {
  override def opposite: Direction = West
}
