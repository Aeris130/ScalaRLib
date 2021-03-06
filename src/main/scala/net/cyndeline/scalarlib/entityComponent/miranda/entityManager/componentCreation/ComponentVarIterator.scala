package net.cyndeline.scalarlib.entityComponent.miranda.entityManager.componentCreation

import net.cyndeline.scalarlib.entityComponent.miranda.Component

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

/**
 * Iterates over every public, non-final var in a component class and calls a method
 * on its name and value. Also iterates over pairs of getters/setters matching those of a var.
 */
class ComponentVarIterator {
   def iterate[T <: Component : TypeTag : ClassTag](component: T, f: (String, FieldMirror) => Unit) {
     val m = ru.runtimeMirror(getClass.getClassLoader)
     val im = m.reflect(component)

     // the component class and all classes it inherits (except java.lang.Object that gets dropped)
     val allClasses = supers(component.getClass).dropRight(1)
     val allSymbols = allClasses.map(c => m.staticClass(c.getCanonicalName))

     // Every term from the class and its parents
     val combinedTerms = allSymbols.map(_.selfType.members).flatten.distinct.filter(_.isTerm).map(_.asTerm)

//     val cSymbol = m.staticClass(component.getClass.getCanonicalName)
//     val componentType = cSymbol.selfType
//     val allTerms = componentType.members.filter(_.isTerm).map(_.asTerm)

     /* Vars and vals are always set to private. Instead two accessor methods are generated by the compiler
      * (name and name_$eq) which have the assigned privacy value. The private vals and vars are also not
      * visible in the scope of a class extending another class, only the public getters and setters are
      * available. Therefore this iterator doesn't distinguish between a var and a pair of accessors
      * matching those that would belong to a var.
      */
     val allAccessors = combinedTerms.filter(m =>
       m.isGetter && // Methods with accessor-like signatures that aren't generated by the compiler won't match this
       !m.isFinal &&
       combinedTerms.exists(t => t == m.setter && combinedTerms.exists(t => t.isVar && t.getter == m.getter)))

     allAccessors.foreach(term => {
       val field: FieldMirror = im.reflectField(term)
       val name = term.name.decoded
       f(name, field)
     })

   }

  def supers(cl: Class[_]): List[Class[_]] = {
    if (cl == null) Nil else cl :: supers(cl.getSuperclass)
  }
}
