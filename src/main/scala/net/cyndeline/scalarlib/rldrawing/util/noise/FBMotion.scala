package net.cyndeline.scalarlib.rldrawing.util.noise

/**
  * Applies fractal brownian motion to a grid using a noise function.
  *
  * Port of the code described by Maher: https://cmaher.github.io/posts/working-with-simplex-noise/
  */
class FBMotion(noiseFunction: Noise2D) {

  /**
    * @param xin X value to generate value for.
    * @param yin Y value to generate value for.
    * @param iterations Number of times to generate noise for x and y before computing the final noise value.
    * @param persistence A value in the range (0, 1). Scales the importance of each subsequent iteration downward.
    * @param scale A value in the range (0, 1). Scales the noise input values by making them smaller, causing the
    *              noise to become smoother. Should be a small value such as 0.1 or 0.007.
    * @return
    */
  def sumOctave(xin: Double, yin: Double, iterations: Int, persistence: Double, scale: Double): Double = {
    require(persistence > 0 && persistence < 1, "Persistence must be in the range (0, 1)")
    require(scale > 0 && scale < 1, "Scaling must be in the range (0, 1)")
    var maxAmp = 0d
    var amp = 1d
    var freq = scale
    var noise = 0d

    for (_ <- 0 until iterations) {
      noise += noiseFunction.noise(xin * freq, yin * freq) * amp
      maxAmp += amp
      amp *= persistence
      freq *= 2
    }

    noise /= maxAmp
    noise
  }

}
