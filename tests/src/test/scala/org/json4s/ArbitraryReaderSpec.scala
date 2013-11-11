package org.json4s

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import JsonDSL._
import org.scalacheck.Arbitrary

/* This borrows heavily from code in Ficus: https://github.com/ceedubs/ficus */
class ArbitraryReaderSpec extends Specification with ScalaCheck {
  import ArbitraryReaderSpec._
  import ArbitraryTypeReader._
  import DefaultReaders.{StringReader, IntReader, OptionReader, traversableReader}

  "An arbitrary type reader" should {
    "instantiate with a single-param apply method" in prop { foo: String =>
      val json: JValue = JObject(JField("foo2", foo))
      val instance: WithSimpleCompanionApply = arbitraryTypeReader[WithSimpleCompanionApply].read(json)
      instance.foo must_== foo
    }

    "instantiate with no apply method but a single constructor with a single param" in prop { foo: String =>
      val json: JValue = JObject(JField("foo", foo))
      val instance: ClassWithSingleParam = arbitraryTypeReader[ClassWithSingleParam].read(json)
      instance.getFoo must_== foo
    }

    "instantiate with a multi-param apply method" in prop { (foo: String, bar: Int) =>
      val json = ("foo" -> foo) ~ ("bar" -> bar)
      val instance: WithMultiCompanionApply = arbitraryTypeReader[WithMultiCompanionApply].read(json)
      (instance.foo must_== foo) and (instance.bar must_== bar)
    }

    "instantiate with no apply method but a single constructor with multiple params" in prop { (foo: String, bar: Int) =>
      val json = ("foo" -> foo) ~ ("bar" -> bar)
      val instance: ClassWithMultipleParams = arbitraryTypeReader[ClassWithMultipleParams].read(json)
      (instance.foo must_== foo) and (instance.bar must_== bar)
    }

    "instantiate with multiple apply methods if only one returns the correct type" in prop { foo: String =>
      val json = JObject(JField("foo", foo))
      val instance: WithMultipleApplyMethods = arbitraryTypeReader[WithMultipleApplyMethods].read(json)
      instance.foo must_== foo
    }

    "instantiate with primary constructor when no apply methods and multiple constructors" in prop { foo: String =>
      val json = JObject(JField("foo", foo))
      val instance: ClassWithMultipleConstructors = arbitraryTypeReader[ClassWithMultipleConstructors].read(json)
      instance.foo must_== foo
    }

    "use another implicit value reader for a field" in {
      val json = JObject(JField("option", "foo"))
      arbitraryTypeReader[WithOption].read(json).option must beSome("foo")
    }

    "fall back to a default value on an apply method" in {
      arbitraryTypeReader[WithDefault].read(JObject()).foo must_== "defaultFoo"
    }

    "fall back to a default value on a constructor arg" in {
      arbitraryTypeReader[ClassWithDefault].read(JObject()).foo must_== "defaultFoo"
    }

    "ignore a default value on an apply method if a value is in config" in prop { foo: String =>
      val json = JObject(JField("foo", foo))
      arbitraryTypeReader[WithDefault].read(json).foo must_== foo
    }

    "ignore a default value in a constructor if a value is in config" in prop { foo: String =>
      val json = JObject(JField("foo", foo))
      arbitraryTypeReader[ClassWithDefault].read(json).foo must_== foo
    }

    "be overridable via implicits" in prop { foo: String =>
      val json = JObject(JField("foo", foo))
      implicit val optionStringReader: Reader[Option[String]] = new Reader[Option[String]] {
        def read(value: JValue): Option[String] = Some("const")
      }
      arbitraryTypeReader[WithOption].read(json).option must beSome("const")
    }

    "read a case class" in prop { instance: SimpleCaseClass =>
      val json = JObject(JField("foo", instance.foo))
      arbitraryTypeReader[SimpleCaseClass].read(json) must beEqualTo(instance)
    }

    "be discoverable by other readers" in prop { instances: List[SimpleCaseClass] =>
      val json = JArray(instances.map(x => JObject(JField("foo", x.foo))))
      json.as[List[SimpleCaseClass]] must beEqualTo(instances)
    }

    "read nested arbitrary types" in prop { withNested: WithNestedCaseClass =>
      val json = JObject(JField("simple", JObject(JField("foo", withNested.simple.foo))))
      arbitraryTypeReader[WithNestedCaseClass].read(json) must beEqualTo(withNested)
    }
  }

}

object ArbitraryReaderSpec {
  trait WithSimpleCompanionApply {
    def foo: String
  }

  object WithSimpleCompanionApply {
    def apply(foo2: String): WithSimpleCompanionApply = new WithSimpleCompanionApply {
      val foo = foo2
    }
  }

  trait WithMultiCompanionApply {
    def foo: String
    def bar: Int
  }

  object WithMultiCompanionApply {
    def apply(foo: String, bar: Int): WithMultiCompanionApply = {
      val (_foo, _bar) = (foo, bar)
      new WithMultiCompanionApply {
        val foo = _foo
        val bar = _bar
      }
    }
  }

  trait WithDefault {
    def foo: String
  }

  object WithDefault {
    def apply(foo: String = "defaultFoo"): WithDefault = {
      val _foo = foo
      new WithDefault {
        val foo = _foo
      }
    }
  }

  trait WithOption {
    def option: Option[String]
  }

  object WithOption {
    def apply(option: Option[String]): WithOption = {
      val _option = option
      new WithOption {
        val option = _option
      }
    }
  }

  class ClassWithSingleParam(foo: String) {
    def getFoo = foo
  }

  class ClassWithMultipleParams(val foo: String, val bar: Int)

  class ClassWithDefault(val foo: String = "defaultFoo")

  trait WithMultipleApplyMethods {
    def foo: String
  }

  object WithMultipleApplyMethods {

    def apply(foo: Option[String]): Option[WithMultipleApplyMethods] = foo map { f =>
      new WithMultipleApplyMethods {
        def foo: String = f
      }
    }

    def apply(foo: String): WithMultipleApplyMethods = {
      val _foo = foo
      new WithMultipleApplyMethods {
        def foo: String = _foo
      }
    }
  }

  class ClassWithMultipleConstructors(val foo: String) {
    def this(fooInt: Int) = this(fooInt.toString)
  }

  case class SimpleCaseClass(foo: String)

  implicit val arbSimpleCaseClass: Arbitrary[SimpleCaseClass] = Arbitrary(Arbitrary.arbitrary[String].map(SimpleCaseClass))

  case class WithNestedCaseClass(simple: SimpleCaseClass)

  implicit val arbWithNestedCaseClass: Arbitrary[WithNestedCaseClass] = Arbitrary(Arbitrary.arbitrary[SimpleCaseClass].map(WithNestedCaseClass))

}
