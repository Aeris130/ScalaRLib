package net.cyndeline.scalarlib.entityComponent.miranda.entityManager.componentCreation

import java.lang.reflect.Constructor

import net.cyndeline.scalarlib.entityComponent.miranda.Component

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

/**
 * A factory that uses reflection to build component factories that trigger an empty constructor
 * when instantiating components.
 */
class ComponentFactoryFactory() {

  def buildFactory[T <: Component : TypeTag](componentClass: Class[T]): ComponentFactory = {
    val constructors: Array[Constructor[_]] = componentClass.getConstructors
    val emptyConstructor = getEmptyConstructor(constructors).getOrElse {
      throw new IllegalArgumentException("Cannot register components that lack a no-argument constructor: " + componentClass.getSimpleName + ". Make sure the component is not an inner class.")
    }

    new NewComponentFactory(emptyConstructor)
  }

  private def getEmptyConstructor(constructors: Array[Constructor[_]]): Option[Constructor[_]] = {
    for (c <- constructors) {
      if (c.getParameterTypes.isEmpty)
        return Option(c)
    }

    None
  }
}
