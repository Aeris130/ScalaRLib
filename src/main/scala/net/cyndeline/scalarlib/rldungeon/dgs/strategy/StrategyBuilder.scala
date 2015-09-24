package net.cyndeline.scalarlib.rldungeon.dgs.strategy

import com.escalatesoft.subcut.inject.{BindingModule, Injectable}
import net.cyndeline.rlcommon.util.ProbabilityCollection
import net.cyndeline.scalarlib.rldungeon.common.{Level, Room}
import net.cyndeline.scalarlib.rldungeon.dgs.Parameter
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.help.ParameterAcceptRatio
import net.cyndeline.scalarlib.rldungeon.dgs.strategy.hillclimbing.HillClimbing
import net.cyndeline.scalarlib.rldungeon.grammar.Strategy
import net.cyndeline.scalarlib.rldungeon.grammar.production.LevelProduction
import net.cyndeline.scalarlib.subcut.ProjectConfiguration

import scala.util.Random
import scalax.collection.GraphEdge.UnDiEdge

/**
 * Contains productions and parameters that should be used for a single strategy.
 */
class StrategyBuilder[L <: Level[L, V, E], V <: Room, E[X] <: UnDiEdge[X]] private
  (parameterResponseValidationOpt: ParameterResponderValidation[L, V, E],
   attempts: Int)(implicit val bindingModule: BindingModule) extends Injectable {

  require(attempts > 0, "The number of attempts to generate a valid level must be equal or greater than 1.")

  /**
   * @param parameters Parameters used to evaluate the modifications of the level.
   * @param productions Productions used to modify the level. Every production is tupled with a value representing how
   *                    likely the production is to be selected at random. A production with value 2.5 will be twice as
   *                    likely to be chosen as one with 1.25. Probability values must be greater than 0.
   * @param random Random object in charge of selecting the order in which productions are applied.
   * @return A strategy object that attempts to output a valid level.
   */
  def createStrategy(parameters: Vector[Parameter[L, V, E]],
                     productions: Vector[(Double, LevelProduction[L, V, E])],
                     random: Random): Strategy[L, V, E] = {
    require(!parameters.isEmpty, "Cannot create a strategy without supplying at least one parameter.")
    require(!productions.isEmpty, "Cannot create a strategy without supplying at least one production.")

    val randomProducts = new ProbabilityCollection[LevelProduction[L, V, E]](random)
    for (kv <- productions)
      randomProducts.add(kv._1, kv._2)

    new HillClimbing[L, V, E](parameters.toSet, randomProducts, parameterResponseValidationOpt, attempts)
  }

}

/**
 * User object in charge of producing strategy builders.
 */
object StrategyBuilder {

  /**
   * @return A strategy builder that attempts to produce a level at most 10 times, with a parameter acceptance ratio
   *         of 1%.
   */
  def apply[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]]() = {
    implicit val module = ProjectConfiguration
    new StrategyBuilder(new ParameterAcceptRatio[L, R, C](0.01), 10)
  }

  /**
   * @param attempts The number of times the algorithm starts over if it fails to produce a valid level.
   * @return A strategy builder that requires that the number of parameters that accept a level exceeds the ones
   *         rejecting it by 1%.
   */
  def apply[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]](attempts: Int) = {
    implicit val module = ProjectConfiguration
    new StrategyBuilder(new ParameterAcceptRatio[L, R, C](0.01), attempts)
  }

  /**
   * @param attempts The number of times the algorithm starts over if it fails to produce a valid level.
   * @param ratio The percent of accepting parameters (0.01 == 1%) that the rejecting parameters must be exceeded
   *              by in order for a level to validate.
   * @return A strategy builder that requires that the number of parameters that accept a level exceeds the ones
   *         rejecting it by the amount specified by the user.
   */
  def apply[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]](attempts: Int, ratio: Double) = {
    implicit val module = ProjectConfiguration
    new StrategyBuilder(new ParameterAcceptRatio[L, R, C](ratio), attempts)
  }

  /**
   * @param attempts The number of times the algorithm starts over if it fails to produce a valid level.
   * @param paramAccept A custom parameter validation object.
   * @return A strategy builder that accepts or rejects a level based on an algorithm supplied by the user.
   */
  def apply[L <: Level[L, R, C], R <: Room, C[X] <: UnDiEdge[X]](attempts: Int, paramAccept: ParameterResponderValidation[L, R, C]) = {
    implicit val module = ProjectConfiguration
    new StrategyBuilder(paramAccept, attempts)
  }

}
