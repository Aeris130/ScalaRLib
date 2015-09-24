package entityComponent.miranda

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import net.cyndeline.scalarlib.entityComponent.miranda.entityManager.componentCreation.ComponentVarIterator
import net.cyndeline.scalarlib.entityComponent.miranda.Component
import scala.reflect.runtime.universe._

@RunWith(classOf[JUnitRunner])
class ComponentVarIteratorSpec extends FunSpec with GivenWhenThen with ShouldMatchers {

  def varIterator = new {
    val iterator = new ComponentVarIterator()
    var storedResult = Map[String, Any]()

    def resultStore(name: String, f: FieldMirror) = {
      storedResult += (name -> f.get)
    }
  }

  describe("ComponentVarIterator") {

    it ("should iterate over all public vars") {

      Given("a component var iterator")
      val it = varIterator
      import it._

      When("parsing a component with 3 vars with name x,y,z and values 1,2,3")
      val comp: ComponentWithVars = new ComponentWithVars()
      iterator.iterate(comp, resultStore)

      Then("the result should contain all three vars an entries")
      storedResult should have size (3)
      storedResult("x") should be (1)
      storedResult("y") should be (2)
      storedResult("z") should be (3)

    }

    it ("should iterate over vars hidden behind the component trait") {

      Given("a component var iterator")
      val it = varIterator
      import it._

      When("parsing a component with 3 vars with name x,y,z and values 1,2,3 not visible from behind the component trait")
      val comp: Component = new ComponentWithVars()
      iterator.iterate(comp, resultStore)

      Then("the result should contain all three vars an entries")
      storedResult should have size (3)
      storedResult("x") should be (1)
      storedResult("y") should be (2)
      storedResult("z") should be (3)

    }

    it ("should iterate over vars from abstract classes") {

      Given("a component var iterator")
      val it = varIterator
      import it._

      When("parsing a component that extends an abstract component class with its own var names abstractVar")
      val comp: Component = new ComponentWithAbstract()
      iterator.iterate(comp, resultStore)

      Then("the result should contain vars from both the component class and its abstract")
      storedResult should have size (2)
      storedResult("abstractVar") should be ("abstract")
      storedResult("localVar") should be (1)

    }

    it ("should not iterate over final vars") {

      Given("a component var iterator")
      val it = varIterator
      import it._

      When("parsing a component with a final var")
      val comp = new ComponentFinalVar()
      iterator.iterate(comp, resultStore)

      Then("the result should be empty")
      storedResult should be ('empty)

    }

    it ("should not parse vals") {

      Given("a component var iterator")
      val it = varIterator
      import it._

      When("parsing a component with a val")
      val comp = new ComponentWithVal()
      iterator.iterate(comp, resultStore)

      Then("the result should be empty")
      storedResult should be ('empty)

    }

    it ("should parse private vars") {

      Given("a component var iterator")
      val it = varIterator
      import it._

      When("parsing a component with a private var")
      val comp = new ComponentPrivateVar()
      iterator.iterate(comp, resultStore)

      Then("the result should be empty")
      storedResult("f") should be ("private")

    }

    it ("should not parse public methods") {

      Given("a component var iterator")
      val it = varIterator
      import it._

      When("parsing a component with a public method")
      val comp = new ComponentPublicMethod()
      iterator.iterate(comp, resultStore)

      Then("the result should be empty")
      storedResult should be ('empty)

    }

    it ("should not parse methods matching the accessors of a var") {

      Given("a component var iterator")
      val it = varIterator
      import it._

      When("parsing a component with a methods acc: Int and acc_=(Int)")
      val comp: ComponentAccessorMethods = new ComponentAccessorMethods()
      iterator.iterate(comp, resultStore)

      Then("the name of the accessor should be stored")
      storedResult should have size (0)

    }

  }
}

class ComponentWithVars extends Component {
  var x: Int = 1
  var y = 2
  var z = 3
}

class ComponentFinalVar extends Component {
  final var f = "final"
}

class ComponentWithVal extends Component {
  val v = 3
}

class ComponentPrivateVar extends Component {
  private var f = "private"
}

class ComponentPublicMethod extends Component {
  def x: Int = 0
  def x_=(newV: Int) = {}
}

class ComponentAccessorMethods extends Component {
  def acc: Int = 0
  def acc_=(m: Int) = {}
}

abstract class AbstractComponent extends Component {
  var abstractVar = "abstract"
}

class ComponentWithAbstract extends AbstractComponent {
  var localVar = 1
}
