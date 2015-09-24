package net.cyndeline.scalarlib.rldrawing.common

/**
 * Specifies a height in coordinates for a drawn element on a grid map.
 */
trait HeightConstraint {

  /**
   * The number of tiles (where each tile occupies a single coordinate) corresponding to the elements height.
   * @return The height of the element, must be higher than 0.
   */
  def elementHeight: Int

}
