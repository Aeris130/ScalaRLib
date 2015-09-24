package testHelpers

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSpec, GivenWhenThen, Matchers}

/**
 * Provides common imports needed for tests.
 */
abstract class SpecImports extends FunSpec with GivenWhenThen with Matchers with MockFactory {

}
