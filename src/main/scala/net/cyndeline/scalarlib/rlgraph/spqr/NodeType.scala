package net.cyndeline.scalarlib.rlgraph.spqr

sealed trait NodeType

case object Series extends NodeType
case object Parallel extends NodeType
case object Rigid extends NodeType
