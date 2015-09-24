package net.cyndeline.scalarlib.entityComponent.miranda.entityManager.eventCreation

import net.cyndeline.scalarlib.entityComponent.miranda.Event

/**
 * Used to create events while setting them up with data that allows processing in constant time. Always use
 * event factories instead of creating new events manually.
 */
trait EventFactory[T <: Event] {
  def createEvent: T
}
