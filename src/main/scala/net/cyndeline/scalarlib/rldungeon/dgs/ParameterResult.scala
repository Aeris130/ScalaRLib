package net.cyndeline.scalarlib.rldungeon.dgs

/**
 * The status of a graph, estimated by a parameter, in regards to an earlier estimate.
 */
sealed trait ParameterResult

/**
 * The new estimate is closer to the parameters target than the previous or inside the target bounds.
 */
case object Accepted extends ParameterResult

/**
 * The new estimate is further away from the parameters target than before and outside the target bounds.
 */
case object Rejected extends ParameterResult

/**
 * No change in the parameters estimate.
 */
case object Indifferent extends ParameterResult
