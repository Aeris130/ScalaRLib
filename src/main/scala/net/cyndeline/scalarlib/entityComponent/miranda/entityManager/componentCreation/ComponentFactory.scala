package net.cyndeline.scalarlib.entityComponent.miranda.entityManager.componentCreation

import net.cyndeline.scalarlib.entityComponent.miranda.Component

/**
 * Produces components with a default set of data.
 */
trait ComponentFactory {
  def produceComponent: Component
}
