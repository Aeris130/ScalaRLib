package net.cyndeline.scalarlib.rldrawing.util.noise

/**
  * Applies modifications to a 2D grid with noise values. Masks take the entire grid as input, as some masks may
  * want to use the dimensions of the grid in their computations.
  */
trait NoiseMask2D {

  /**
    *
    * Matrix with noise values to apply mask onto. Every value should be in the range [0, 1].
    * @return A copy of the noise grid with the mask applied.
    */
  def applyMask(noiseGrid: Array[Array[Double]]): Array[Array[Double]]

}
