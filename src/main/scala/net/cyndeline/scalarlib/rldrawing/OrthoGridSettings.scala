package net.cyndeline.scalarlib.rldrawing

/**
 * User-specified settings for generating orthogonal grid layouts.
 */
//TODO make immutable when finilized
class OrthoGridSettings {

  /** True if the walls of two adjacent rooms should be allowed to intersect, otherwise false. Allowing intersections
    * gives the algorithm more space to optimize the map density.
    */
  var wallIntersects: Boolean = true

}
