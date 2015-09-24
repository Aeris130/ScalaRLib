package net.cyndeline.scalarlib.entityComponent.miranda

/**
 * Supplied to the entity manager to create an entity with a predetermined component set loaded with default data.
 * Saves the user the need to manually set components wherever an entity of a particular type is created.
 *
 * @param name Name of the entity being created (e.g "Orc", "Tank", "Bullet" etc). For debugging only.
 * @param description A short description of the entity. For debugging only.
 * @param id Used by the entity manager to select component set.
 */
protected case class Assemblage(name: String, description: String, id: Int)
