package net.cyndeline.scalarlib.entityComponent.miranda

/**
 * Hides implementation-specific data related to components, giving the user a single object to use when
 * referring to a specific component type.
 *
 * @param componentId An integer id that allows constant-time lookups of the given component type in
 *                    the component table.
 * @param name The name of the class represented by the component type object. Used for debugging only.
 * @tparam T Component class represented by this object.
 */
class ComponentType[T <: Component](val componentId: Int, val name: String) {

}
