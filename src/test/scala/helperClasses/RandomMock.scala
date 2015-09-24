package helperClasses

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
 * Scalamock cannot mock Random objects from 2.11. This is a substitute.
 */
class RandomMock() extends Random {
  val nextInts = new ListBuffer[Int]()
  val nextIntExpectation = new ListBuffer[RMExpectationReturn]()
  val nextgaussians = new ListBuffer[Double]()

  def nextIntReturns(i: Int): RandomMock = {
    nextInts += i
    this
  }

  def expects(i: Int) = RMExpectation(i, this)

  def nextGaussianReturns(d: Double) = {
    nextgaussians += d
    this
  }

  override def nextInt: Int = if (nextInts.nonEmpty) {
    val ret = nextInts.head
    nextInts.remove(0)
    ret
  } else {
    throw new Error("No more Ints to return.")
  }

  override def nextInt(i: Int): Int = if (nextIntExpectation.nonEmpty) {
    val ret = nextIntExpectation.head
    require(ret.expectation == i, "Max int value (" + i + ") doesn't match expected value (" + ret.expectation + ")")
    nextIntExpectation.remove(0)
    ret.returnValue
  } else {
    throw new Error("No more Ints to return.")
  }

  override def nextGaussian(): Double = if (nextgaussians.nonEmpty) {
    val r = nextgaussians.head
    nextgaussians.remove(0)
    r
  } else {
    throw new Error("No more doubles to return.")
  }
}

case class RMExpectation(i: Int, mock: RandomMock) {
  def returns(r: Int): RMExpectationReturn = {
    val rv = RMExpectationReturn(i, r)
    mock.nextIntExpectation += rv
    rv
  }
}
case class RMExpectationReturn(expectation: Int, returnValue: Int)

object RandomMock {
  def apply(): RandomMock = new RandomMock()
}
