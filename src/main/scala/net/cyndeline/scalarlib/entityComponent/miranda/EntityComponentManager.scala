package net.cyndeline.scalarlib.entityComponent.miranda

import net.cyndeline.scalarlib.entityComponent.miranda.entityManager.eventCreation.EventFactory

import scala.reflect.runtime.universe._

/**
 * Main interaction point for creating/deleting/storing/retrieving entity/component data.
 */
trait EntityComponentManager {

  /**
   * Creates a new entity entry.
   * @return The entity that was just created.
   */
  def createEntity: Entity

  /**
   * Deletes an entity and frees up its id for re-use.
   * @param entity The entity to remove.
   */
  def deleteEntity(entity: Entity)

  /**
   * Assembles an entity with a predetermined set of components.
   * @param assemblage Assembly object representing the type of entity to be created.
   * @return The entity that was just created.
   */
  def assembleEntity(assemblage: Assemblage): Entity

  /**
   * Assembles one or more entities based on a blueprint where each such entity is mapped to a String key.
   * @param blueprint The blueprint to use when creating entities.
   * @return A mapping between each key and the entity it belongs to that was created.
   */
  def assembleFromBlueprint(blueprint: Blueprint): Map[String, Entity]

  /**
   * Adds a component with default data to an entity.
   *
   * @param entity The entity to add component to.
   * @param key Component key representing the component type to add.
   * @return The component that was added.
   */
  def addComponent[T <: Component](entity: Entity, key: ComponentType[T]): T

  /**
   * Removes a component from an entity.
   * @param entity The entity to remove component from.
   * @param key Component key representing the component type to remove.
   */
  def removeComponent[T <: Component](entity: Entity, key: ComponentType[T])

  /**
   * Retrieves a component from an entity, of the type specified by the component key.
   * @param entity The entity to retrieve component from.
   * @param key Component key representing the component type to retrieve.
   */
  def getComponent[T <: Component](entity: Entity, key: ComponentType[T]): T

  /**
   * Checks if a component is present in an entity.
   * @param entity Entity to check component presence in.
   * @param key Component type to look for.
   * @return True if the entity has a component of the given type, otherwise false.
   */
  def hasComponent[T <: Component](entity: Entity, key: ComponentType[T]): Boolean

  /**
   * Sends an event to an entity.
   * @param entity The entity to send event to.
   * @param event Event to send.
   */
  def sendEvent(entity: Entity, event: Event)

  /**
   * Sends an event to the event handler as a response to another event. Events sent as responses will
   * be processed before any other non-response events sent by other systems during the same process loop.
   *
   * Example: Event A is sent. A is delivered to two systems (1 & 2), and they both send a message to the manager
   * (B & C) in the order B,C. Since B was sent first, it will be the next event to be processed. Any events
   * sent in response to B will be processed before C, and any events sent as responses to those responses will
   * be processed before C and any response that hasn't preceded it.
   *
   * @param entity Entity to send response to.
   * @param eventToReplyWith Event to send as a response.
   * @param eventToReplyTo Event to respond to.
   */
  def replyToEvent(entity: Entity, eventToReplyWith: Event, eventToReplyTo: Event)

  /**
   * Retrieves a key mapped to a specific component type. Do not call this method every time a key is needed,
   * as the lookup time isn't constant.
   * @param componentClass Class to retrieve key for.
   * @return A component key belonging to the specified class.
   */
  def componentType[T <: Component : TypeTag](componentClass: Class[T]): ComponentType[T]

  /**
   * Retrieves a factory used to instantiate events of a particular class. Looking up factories is not done
   * at constant time, do not invoke this method every time an event must be created, or the performance gains
   * from using the factory will be lost.
   *
   * @param eventClass Class to instantiate. Must have an empty constructor.
   * @tparam E Event type.
   * @return A factory that instantiates events of the specified type with default data.
   */
  def eventFactory[E <: Event](eventClass: Class[E]): EventFactory[E]

  /**
   * Subscribes a system to the set of events in its subscription.
   * @param system System to register.
   */
  def registerSystem(system: EventSystem)

  /**
   * Registers a set of components to be added to a newly created entity whenever the resulting assembler object
   * is supplied. Call only once per assemblage object, or duplicates will be created.
   * @param name Name of the entity type created.
   * @param description A short description of the entity.
   * @param componentSet Every component class that should be created and added to the entity.
   * @param dataSet Pre-instantiated components. Every component in this set will be added to the assemblage, and also
   *                have its public variables stored in the manager. When assembling entities with the resulting
   *                assemblage, those variables will have their values set to the data in the component.
   * @return A assembler object that can be passed back to the entity manager when creating entities, resulting in an
   *         entity with the given component set.
   */
  def registerAssemblage(name: String, description: String, componentSet: Set[Class[_ <: Component]], dataSet: Set[Component]): Assemblage

  /**
   * Instantiates a new blueprint. Additional blueprint data can be added to the returned object.
   * @param name Name of the blueprint. For debugging only.
   * @param description A short description of the blueprint. For debugging only.
   * @return A new blueprint with a unique id that can be used to create entities and modify them upon creation.
   */
  def registerBlueprint(name: String, description: String): Blueprint

  /**
   * Adds a listener to the manager that is invoked upon data modification.
   * @param listener Subscriber to entity data modifications.
   */
  def addEntityListener(listener: EntityListener)

  /**
   * Retrieves an event handler that processes events sent using this manager.
   * @return An event handler that can be used to process messages sent using this manager.
   */
  def eventHandler: EventHandler

}
