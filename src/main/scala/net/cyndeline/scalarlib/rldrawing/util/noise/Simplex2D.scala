package net.cyndeline.scalarlib.rldrawing.util.noise

import net.cyndeline.rlcommon.math.Normalize

import scala.util.Random

/**
  * Computes simplex noise in two dimensions.
  *
  * Scala port of Stefan Gustavsons java implementation: http://webstaff.itn.liu.se/~stegu/simplexnoise/
  */
class Simplex2D private (perm: Vector[Short], permMod12: Vector[Short]) extends Noise2D {

  /**
    * Generates a noise value for a single (x,y) coordinate on the 2D plane.
    * @param xin X value to generate value for.
    * @param yin Y value to generate value for.
    * @return A value in the range of [0, 1].
    */
  def noise(xin: Double, yin: Double): Double = {
    var n0, n1, n2: Double = 0

    // Skew the input space to determine which simplex cell we're in
    val s: Double = (xin + yin) * Simplex2D.F2 // Hairy factor for 2D
    val i: Int = Simplex2D.fastFloor(xin+s)
    val j: Int = Simplex2D.fastFloor(yin+s)
    val t: Double = (i + j) * Simplex2D.G2
    val X0: Double = i - t // Unskew the cell origin back to (x,y) space
    val Y0: Double = j - t
    val x0: Double = xin - X0
    val y0: Double = yin - Y0

    // For the 2D case, the simplex shape is an equilateral triangle.
    // Determine which simplex we are in.
    var i1, j1: Int = 0 // Offsets for second (middle) corner of simplex in (i,j) coords
    if (x0 > y0) { i1=1; j1=0 } // lower triangle, XY order: (0,0)->(1,0)->(1,1)
    else { i1=0; j1=1 }      // upper triangle, YX order: (0,0)->(0,1)->(1,1)

    // A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
    // a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
    // c = (3-sqrt(3))/6

    val x1: Double = x0 - i1 + Simplex2D.G2 // Offsets for middle corner in (x,y) unskewed coords
    val y1: Double = y0 - j1 + Simplex2D.G2
    val x2: Double = x0 - 1.0 + 2.0 * Simplex2D.G2 // Offsets for last corner in (x,y) unskewed coords
    val y2: Double = y0 - 1.0 + 2.0 * Simplex2D.G2

    // Work out the hashed gradient indices of the three simplex corners
    val ii: Int = i & 255
    val jj: Int = j & 255
    val gi0: Int = permMod12(ii + perm(jj))
    val gi1: Int = permMod12(ii + i1 + perm(jj + j1))
    val gi2: Int = permMod12(ii + 1 + perm(jj + 1))

    // Calculate the contribution from the three corners
    var t0: Double = 0.5 - x0 * x0 - y0 * y0
    if (t0 < 0)
      n0 = 0.0
    else {
      t0 *= t0
      n0 = t0 * t0 * Simplex2D.dot(Simplex2D.grad3(gi0), x0, y0)  // (x,y) of grad3 used for 2D gradient
    }

    var t1: Double = 0.5 - x1 * x1 - y1 * y1
    if (t1 < 0) n1 = 0.0
    else {
      t1 *= t1
      n1 = t1 * t1 * Simplex2D.dot(Simplex2D.grad3(gi1), x1, y1)
    }

    var t2: Double = 0.5 - x2 * x2 - y2 * y2
    if (t2 < 0) n2 = 0.0
    else {
      t2 *= t2
      n2 = t2 * t2 * Simplex2D.dot(Simplex2D.grad3(gi2), x2, y2)
    }

    // Add contributions from each corner to get the final noise value.
    // The result is scaled to return values in the interval [-1,1].
    Normalize.apply(70.0 * (n0 + n1 + n2), -1, 1) // Custom normalization to [0, 1]

  }

}

object Simplex2D {

  /**
    * Constructs a new seeded simplex generator.
    * @param seed Seed that determines how the simplex values should be generated.
    */
  def apply(seed: Int): Simplex2D = {
    val r = new Random(seed)
    val p: Vector[Short] = r.shuffle(for (i <- 0 to 255) yield i.toShort).toVector
    var perm = Vector.fill[Short](512)(0)
    var permMod12 = Vector.fill[Short](512)(0)

    for (i <- 0 until 512) {
      perm = perm.updated(i, p(i & 255))
      permMod12 = permMod12.updated(i, (perm(i) % 12).toShort)
    }

    new Simplex2D(perm, permMod12)
  }

  private case class Grad(x: Double, y: Double, z: Int)

  private val grad3: Vector[Grad] = Vector(Grad(1,1,0), Grad(-1,1,0), Grad(1,-1,0),
    Grad(-1,-1,0), Grad(1,0,1), Grad(-1,0,1), Grad(1,0,-1), Grad(-1,0,-1),
    Grad(0,1,1), Grad(0,-1,1), Grad(0,1,-1), Grad(0,-1,-1))

  //TODO Moved this to constructor to make noise random, delete when class is confirmed working
//  private val p: Vector[Short] = Vector(151,160,137,91,90,15,
//    131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
//    190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
//    88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
//    77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
//    102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
//    135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
//    5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
//    223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
//    129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
//    251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
//    49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
//    138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180)
//
//  private var perm = Vector.fill[Short](512)(0)
//  private var permMod12 = Vector.fill[Short](512)(0)
//
//  for (i <- 0 until 512) {
//    perm = perm.updated(i, p(i & 255))
//    permMod12 = permMod12.updated(i, (perm(i) % 12).toShort)
//  }

  private val F2: Double = 0.5 * (Math.sqrt(3.0) - 1.0)
  private val G2: Double = (3.0 - Math.sqrt(3.0)) / 6.0

  private def fastFloor(x: Double) = {
    val xi: Int = x.toInt
    if (x < xi) xi-1 else xi
  }

  private def dot(g: Grad, x: Double, y: Double) = (g.x * x) + (g.y * y)

}
