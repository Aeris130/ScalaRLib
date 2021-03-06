package net.cyndeline.scalarlib.rldungeon.dgs.strategy.hillclimbing.modules

import com.escalatesoft.subcut.inject.{BindingModule, Injectable}
import net.cyndeline.rlcommon.util.RandomCollection
import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}
import net.cyndeline.scalarlib.rldungeon.dgs.Parameter
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.ParameterResponderValidation
import net.cyndeline.scalarlib.rldungeon.grammar.production.LevelProduction

import scala.language.higherKinds
import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef.EdgeLikeIn

/**
 * Used for injection.
 */
trait ProductionIteratorI {
  def applyProductions[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X], PV]
    (level: L,
     parameters: Set[Parameter[L, R, C]],
     productions: RandomCollection[LevelProduction[L, R, C, PV]],
     paramResponseValidation: ParameterResponderValidation[L, R, C],
     r: Random): L
}

/**
 * Selects a random production and applies it until the parameter set no longer accepts further modifications.
 * Productions that doesn't yield results (i.e are rejected by the parameters) are removed until one is found
 * that does (reset the production set) or every production has been tried (exit).
 *
 * @constructor Constructs a new production iterator.
 */
class ProductionIterator(implicit val bindingModule: BindingModule)
  extends ProductionIteratorI with Injectable {

  private val validator = injectOptional[ValidatorI] getOrElse { new Validator() }

  /**
   * Modifies a level using productions and parameters.
   *
   * @param level Level to modify.
   * @param parameters Every parameter that specifies which modifications to the map are considered beneficial to
   *                   reaching the target parameter value.
   * @param productions Objects that modifies a level.
   * @param paramResponseValidation Takes the set of rejecting and accepting parameters and decides if a modification
   *                                should be kept or discarded.
   * @param r Random object used to select productions.
   * @return The modified level, or the input level if no modifications were accepted.
   */
  def applyProductions[L <: Level[L, R, C], R <: Room, C[X] <: EdgeLikeIn[X], PV]
    (level: L,
    parameters: Set[Parameter[L, R, C]],
    productions: RandomCollection[LevelProduction[L, R, C, PV]],
    paramResponseValidation: ParameterResponderValidation[L, R, C],
    r: Random): L = {

    var currentProductions = productions
    var currentLevel = level
    var estimates: Map[Parameter[L, R, C], Double] = (for (p <- parameters) yield p -> 0.0).toMap

    while (!currentProductions.isEmpty) {
      val productionToTry = currentProductions.next(r)
      val modification: Option[L] = productionToTry.apply(currentLevel)

      if (modification.isDefined) {
        val validatingParameters = validator.validateModifiedGraph(parameters, estimates, modification.get)
        val acceptingParams = validatingParameters._1
        val rejectingParams = validatingParameters._2

        /* Only keep the new graph if some parameter accepted it, and the number of parameters that did that
         * outnumbers the ones that didn't by a set percentage. If a priority-parameter rejects the new graph,
         * discard it regardless.
         */
        if (!rejectingParams.exists(p => p.hasPriority) && paramResponseValidation.levelModificationValidates(acceptingParams, rejectingParams)) {
          estimates = validatingParameters._3
          currentLevel = modification.get

          // Reset the production collection if one or more elements has been removed
          if (currentProductions.size < productions.size)
            currentProductions = productions
        } else {
          currentProductions = currentProductions.remove(productionToTry)
        }
      } else {
        currentProductions = currentProductions.remove(productionToTry)
      }
    }

    currentLevel
  }
}
