package net.cyndeline.scalarlib.entityComponent.miranda

/**
 * Tags any message as being able to be sent to an entity. Users can ignore methods here.
 */
abstract class Event {
  private var idValue = -1
  private var cancelStatus = false

  /* Stores how many events that proceeds this one in a chain of events sent as replies to each other.
   * Example: Event A is sent, B is sent as reply to A, and this event is sent as reply to B. This would
   * give this event the reply order orderOf(B) + 1 = 2. Every event that isn't sent as a reply (in this case A)
   * has the order 0.
   *
   * This value is only modified by the event manager. Don't touch.
   */
  var replyOrder = 0

  def id: Int = if (idValue >= 0) idValue else throw new Error("The event " + this + " hasn't had its id initiated. Do not instantiate events manually.")

  def id_=(newId: Int) {
    if (idSet)
      throw new Error("Cannot set event ids more than once.")
    else if (newId < 0)
      throw new Error("Event ids cannot be negative.")

    idValue = newId
  }

  def isCancelled: Boolean = cancelStatus

  def cancel() { cancelStatus = true }

  private def idSet: Boolean = idValue >= 0
}
