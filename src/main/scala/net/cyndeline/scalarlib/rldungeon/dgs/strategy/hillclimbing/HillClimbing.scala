package net.cyndeline.scalarlib.rldungeon.dgs.strategy.hillclimbing

import com.escalatesoft.subcut.inject.{BindingModule, Injectable}
import net.cyndeline.rlcommon.util.RandomCollection
import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}
import net.cyndeline.scalarlib.rldungeon.dgs.Parameter
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.ParameterResponderValidation
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.hillclimbing.modules.{ProductionIterator, ProductionIteratorI}
import net.cyndeline.scalarlib.rldungeon.grammar.Strategy
import net.cyndeline.scalarlib.rldungeon.grammar.production.LevelProduction

import scalax.collection.GraphEdge.UnDiEdge

/**
 * Applies productions randomly in a hill-climbing fashion according to the following algorithm:
 *
 *  1. Start with a set of productions to choose from, none of which has been tried yet
 *  1. Select a production randomly and apply it, remove it from the production set
 *   - If the production didn't yield a result, select another and remove it until one is found that does.
 *   - If no yielding production was found (production set is empty), no more modifications can be performed:
 *   Do Exit step.
 *  1. Compare the resulting level from the production with the previous level using every parameters estimator object.
 *  If the responder validation accepts the modification based on the accepting and rejecting parameters, the change is
 *  kept.
 *   - If any parameter with priority rejects the change, the change is not kept and another production is selected.
 *  1. If the production was successfully applied and accepted, the set of productions are reset to its initial state,
 *  and the process starts over.
 *  1. When no more modifications are made (due to no production yielding improvements, the parameter set either not
 *  agreeing on which change to keep, or because every parameter is content with the current result), run a final check
 *  to ensure that enough parameters accept the final graph (by the above mentioned responder validation) and that no
 *  priority parameters reject it.
 *   - If the level is not accepted, start over. Otherwise return it.
 *
 * @constructor Constructs a new hill climber instance.
 * @param parameters Every parameter that should be used when estimating production validity. Cannot contain duplicates.
 * @param productions All productions to be used when producing the graph, along with the probability weight
 *                     determining how likely that production is to be applied. Example: 3 productions A, B, C having
 *                     weights 1.0, 1.0 and 2.0 makes A and B 25% likely to be chosen each, and C 50% likely.
 * @param paramValidation Validator responsible for deciding if a change to the map should be kept or discarded.
 * @param attempts If the final level is not accepted, the algorithm starts over. This number controls how many times
 *                 that is allowed to happen before the hill climber gives up and returns None.
 */
class HillClimbing[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]]
  (parameters: Set[Parameter[L, R, C]],
  productions: RandomCollection[LevelProduction[L, R, C]],
  paramValidation: ParameterResponderValidation[L, R, C],
  attempts: Int)
  (implicit val bindingModule: BindingModule)
  extends Strategy[L, R, C]
  with Injectable {

  private val productionIterator = injectOptional[ProductionIteratorI] getOrElse {
    new ProductionIterator()
  }

  /**
   * Produces a level.
   *
   * @param level The graph level the productions initially will be applied to.
   * @return the finished map graph if one was possible to produce according to the strategy, otherwise None.
   */
  override def apply(level: L): Option[L] = {
    var attemptsLeft = attempts

    while (attemptsLeft > 0) {

      // Either the input graph if no modifications passed, or the modified graph.
      val finalLevel = productionIterator.applyProductions[L, R, C](level, parameters, productions, paramValidation)

      /* No more modifications ended up being approved by the parameters. Run a final check to see if the graph is
       * valid (exit) or not (try again). Note that this check only runs the graphs estimated value against
       * the parameter target value range, no pre/post modification estimates are involved.
       */
      val acceptingParameters = for {
        p <- parameters
        if p.validate(finalLevel)
      } yield p

      val rejectingParameters = (parameters diff acceptingParameters).toSet
      if (!rejectingParameters.exists(p => p.hasPriority) && paramValidation.levelModificationValidates(acceptingParameters.toSet, rejectingParameters))
        return Option(finalLevel)
      else
        attemptsLeft -= 1
    }

    None
  }
}
