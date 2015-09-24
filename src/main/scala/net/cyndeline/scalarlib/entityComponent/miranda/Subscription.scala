package net.cyndeline.scalarlib.entityComponent.miranda

/**
 * Added to event systems to denote which events they're subscribed to, and which components must be
 * present on an entity to process it.
 *
 * @param events All events that the system should subscribe to.
 */
class Subscription(val events: Set[Class[_ <: Event]]) {
  private var includedComponents = Set[Class[_ <: Component]]()

  /**
   * @return The set of components for which every component must be present in an entity in order for the system to
   *         process it.
   */
  def includeAll: Set[Class[_ <: Component]] = includedComponents

  /**
   * Add a component to the inclusion set.
   * @param c Component class to include.
   */
  def includeComponent(c: Class[_ <: Component]) {
    includedComponents += c
  }

  /* Component types that match the classes supplied by the user. These types won't be available until the subscription
   * has been processed by the entity manager, and stores component ids that allow lookups to be performed in constant
   * time.
   *
   * Set by the entity manager, don't touch! */
  var includeAllTypes = Set[ComponentType[_ <: Component]]()
}
