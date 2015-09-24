package net.cyndeline.scalarlib.entityComponent.miranda.entityManager.componentCreation

import java.lang.reflect.Constructor
import net.cyndeline.scalarlib.entityComponent.miranda.Component

/**
 * Creates new components from scratch.
 *
 * @param constructor Used to create new components.
 */
class NewComponentFactory(constructor: Constructor[_]) extends ComponentFactory {
  def produceComponent: Component = constructor.newInstance().asInstanceOf[Component]
}
