package net.cyndeline.scalarlib.entityComponent.miranda.entityManager

import net.cyndeline.scalarlib.entityComponent.miranda.util.Bag
import net.cyndeline.scalarlib.entityComponent.miranda.{ComponentType, Component}

/**
 * Stores an array of entities, where an entity is represented by its ID (its index in the array) mapped
 * towards its own Component-array, an array where each index represents a component id. This allows constant
 * time lookups.
 */
class ComponentTable {
  private val entities = new Bag[Bag[Component]]()

  /**
   * Registers an entity as being able to have components.
   * @param entity The id of the entity to register.
   */
  def registerEntity(entity: Int) {
    val componentsForEntity: Option[Bag[Component]] = entities.get(entity)
    if (componentsForEntity.isDefined)
      throw new Error("The entity with id " + entity + " is already registered in the component table, and cannot be registered again until the id has been cleared.")

    entities.set(entity, new Bag[Component]())
  }

  /**
   * Clears component data for an entity. No more data can be added until the entity id has been
   * registered again.
   * @param entity Entity to clear data for.
   */
  def unregisterEntity(entity: Int) {
    if (entities.remove(entity) == None)
      throw new Error("Entity with id " + entity + " not found in component table, cannot be removed.")
  }

  /**
   * Registers a component to an entity.
   * @param entity Id of entity to add component to.
   * @param component Component to add.
   */
  def addComponent(entity: Int, componentId: Int, component: Component) {
    val componentsForEntity: Option[Bag[Component]] = entities.get(entity)
    if (!componentsForEntity.isDefined)
      throw new Error("The entity " + entity + " did not exist, and could not have a component with id " + componentId + " ( " + component.getClass + ") added to it.")

    val components = componentsForEntity.get
    if (components.get(componentId).isDefined)
      throw new Error("Entity " + entity + " already has a component of type " + component + ".")

    components.set(componentId, component)
  }

  /**
   * Removes a component from an entity.
   * @param entity Id of entity to remove component from.
   * @param component Component to remove.
   */
  def removeComponent[T <: Component](entity: Int, component: ComponentType[T]) {
    val componentsForEntity: Option[Bag[Component]] = entities.get(entity)
    if (!componentsForEntity.isDefined)
      throw new Error("The entity " + entity + " did not exist.")

    val components = componentsForEntity.get
    if (!components.get(component.componentId).isDefined)
      throw new Error("Entity " + entity + " doesn't have a component of type " + component.name + ", it cannot be removed.")

    components.remove(component.componentId)
  }

  /**
   * Retrieves a component.
   * @param entity Entity to retrieve component for.
   * @param componentId Id of component to retrieve.
   * @return The component of the specified type.
   */
  def getComponent(entity: Int, componentId: Int): Component = {
    val componentBag = entities.get(entity).getOrElse {
      throw new Error("Entity with id " + entity + " not found, cannot retrieve components.")
    }

    componentBag.get(componentId).getOrElse {
      throw new Error("No component with id " + componentId + " found in entity " + entity)
    }
  }

  /**
   * Checks if an entity has a component.
   * @param entity Entity to look for component in.
   * @param componentId Id of component to look for.
   * @return True if the entity has the component, otherwise false.
   */
  def hasComponent(entity: Int, componentId: Int): Boolean = {
    val componentBag = entities.get(entity).getOrElse {
      throw new Error("Entity with id " + entity + " not found, cannot check if component exists.")
    }

    componentBag.get(componentId).isDefined
  }
}
