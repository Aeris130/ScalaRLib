package net.cyndeline.scalarlib.rlgraph.triconnectivity.minMaxDegree.aug.rigid

/**
 * The two types of face sets.
 */
sealed trait FaceSetType

case object OneSet extends FaceSetType
case object TwoSet extends FaceSetType
