package net.cyndeline.scalarlib.entityComponent.miranda.entityManager.eventCreation

import net.cyndeline.scalarlib.entityComponent.miranda.Event
import java.lang.reflect.Constructor

/**
 * Creates factories responsible for instantiating events with data that allows them to
 * be processed in constant time.
 */
class EventFactoryFactory {

  def buildFactory[E <: Event](eventClass: Class[E], id: Int): EventFactory[E] = {
    val constructors: Array[Constructor[_]] = eventClass.getConstructors
    val emptyConstructor = getEmptyConstructor(constructors).getOrElse {
      throw new Error("The event class did not define an empty constructor: " + eventClass + ".")
    }

    new NewEventFactory[E](emptyConstructor, id)
  }

  private def getEmptyConstructor(constructors: Array[Constructor[_]]): Option[Constructor[_]] = {
    for (c <- constructors) {
      if (c.getParameterTypes.isEmpty)
        return Option(c)
    }

    None
  }
}
