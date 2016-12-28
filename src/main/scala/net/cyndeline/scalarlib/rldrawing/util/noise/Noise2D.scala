package net.cyndeline.scalarlib.rldrawing.util.noise

/**
  * Created by Tobias Edin on 2016-12-12.
  */
trait Noise2D {

  /**
    * Generates a noise value for a single (x,y) coordinate on the 2D plane.
    * @param xin X value to generate value for.
    * @param yin Y value to generate value for.
    * @return A value in the range of [0, 1].
    */
  def noise(xin: Double, yin: Double): Double

}
