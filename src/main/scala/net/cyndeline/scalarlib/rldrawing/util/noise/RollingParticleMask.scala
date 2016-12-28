package net.cyndeline.scalarlib.rldrawing.util.noise

import net.cyndeline.rlcommon.math.Normalize

import scala.util.Random

/**
  * Applies a mask to a noise matrix, reducing the noise towards the outer borders of the matrix. The algorithm proceeds
  * as follows:
  *
  *  1. Create a matrix with the same dimensions as the one containing the noise, fill it with 0's.
  *  1. Spawn a particle on a random cell in the matrix and increase the cells value by 1.
  *  1. Move the particle to another cell whose value is equal or lower than the current cell (causing it to "roll
  *  downhill"), increase that cells value by 1. Repeat this step until the particles lifespan (measured in number of
  *  steps) is up, or until no more valid steps may be made.
  *  1. Repeat from step 2 until all particles have been spawned and processed.
  *  1. Normalize the values in the grid to the range [0, 1], then multiply the noise with the corresponding particle
  *  value (will be 1 towards the center of the grid, and 0 towards the border).
  *
  * Particles are spawned towards the center of the matrix to round out the edges. To ensure that the noise around the
  * border is kept to a minimum, the outermost and second outermost rows/columns have their noise reduced manually.
  * This, combined with the fact that those cells will have the lowest noise to begin with, minimizes the outermost
  * noise.
  *
  * @param particles Number of particles to spawn.
  * @param particleLife Number of turns a particle stays active.
  * @param seed Ensures deterministic movement behaviour.
  */
class RollingParticleMask(particles: Int, particleLife: Int, seed: Int) {
  private val margin = 0.1 // 10% on both sides = 20%

  /**
    * @param noiseGrid Matrix with noise values to apply mask onto.
    * @return A copy of the noise grid with the mask applied. See class description.
    */
  def applyMask(noiseGrid: Array[Array[Double]]): Array[Array[Double]] = {
    require(noiseGrid.length > 0, "Cannot apply mask to empty grid.")
    val r = new Random(seed)
    val width = noiseGrid.length
    val height = noiseGrid(0).length

    val particleGrid = Array.fill(width, height)(0)

    var i = 0
    while (i < particles) {
      val particleStart = spawnPoint(width, height, r)
      moveParticle(particleStart, particleGrid, width, height, r)
      i += 1
    }

    // Normalize to [0, 1]
    var lowest = Int.MaxValue
    var highest = Int.MinValue
    var m = 0
    while (m < width) {
      var n = 0
      while (n < height) {
        val value = particleGrid(m)(n)
        if (value < lowest)
          lowest = value
        if (value > highest)
          highest = value
        n += 1
      }
      m += 1
    }

    val normalizedGrid = Array.fill(width, height)(0d)
    m = 0
    while (m < width) {
      var n = 0
      while (n < height) {
        normalizedGrid(m)(n) = Normalize(particleGrid(m)(n), lowest, highest)
        n += 1
      }
      m += 1
    }

    // Apply mask
    val maskedNoiseGrid = Array.fill(width, height)(0d)
    m = 0
    while (m < width) {
      var n = 0
      while (n < height) {
        maskedNoiseGrid(m)(n) = noiseGrid(m)(n) * normalizedGrid(m)(n)
        n += 1
      }
      m += 1
    }

    maskedNoiseGrid
  }

  private def moveParticle(start: (Int, Int), grid: Array[Array[Int]], width: Int, height: Int, r: Random): Unit = {
    var current = (0, 0)
    def setAndIncrement(position: (Int, Int)): Unit = {
      current = position
      grid(current._1)(current._2) += 1
    }
    setAndIncrement(start)

    var i = 0
    while (i < particleLife) {
      val possibleMoves = validMoves(current, grid, width, height)

      if (possibleMoves.isEmpty) {
        i = particleLife
      } else {
        val move = possibleMoves(r.nextInt(possibleMoves.length))
        setAndIncrement((current._1 + move._1, current._2 + move._2))
        i += 1
      }
    }
  }

  private def validMoves(current: (Int, Int), grid: Array[Array[Int]], width: Int, height: Int): Seq[(Int, Int)] = {
    val currentValue = grid(current._1)(current._2)
    val allMoves = for (
      x <- -1 to 1;
      y <- -1 to 1
      if (x != 0 || y != 0) && // Don't want the particle to move to its current position
        current._1 + x >= 0 && current._1 + x < width &&
        current._2 + y >= 0 && current._2 + y < height
    ) yield (x, y)

    allMoves.filter(move => grid(current._1 + move._1)(current._2 + move._2) <= currentValue)
  }

  /*
   * Doesn't generate points in the outer 20% of the grid.
   */
  private def spawnPoint(width: Int, height: Int, r: Random): (Int, Int) = {
    val wMargin = width * margin
    val hMargin = height * margin
    val x = r.nextInt((width * (margin * 2)).toInt) + wMargin
    val y = r.nextInt((height * (margin * 2)).toInt) + hMargin
    (x.toInt, y.toInt)
  }

}
