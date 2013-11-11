package org.json4s

import scala.language.experimental.macros
import scala.reflect.internal.{StdNames, SymbolTable, Definitions}
import org.json4s.reflect.macros.ReflectionUtils

/* This borrows heavily from code in Ficus: https://github.com/ceedubs/ficus */
trait ArbitraryTypeReader {
  implicit def arbitraryTypeReader[T]: Reader[T] = macro ArbitrayTypeReaderMacros.arbitraryTypeReader[T]
}

object ArbitraryTypeReader extends ArbitraryTypeReader

object AllTypeReaders extends ArbitraryTypeReader with DefaultReaders

object ArbitrayTypeReaderMacros {
  import scala.reflect.macros.Context

  def arbitraryTypeReader[T : c.WeakTypeTag](c: Context): c.Expr[Reader[T]] = {
    import c.universe._

    val returnType = c.weakTypeOf[T]

    def fail(reason: String) = c.abort(c.enclosingPosition, s"Cannot generate a reader for type $returnType, because $reason")

    val companionSymbol = returnType.typeSymbol.companionSymbol match {
      case NoSymbol => None
      case x => Some(x)
    }

    reify {
      new Reader[T] {
        def read(value: JValue): T = parseJValue[T](c)(
          jValue = c.Expr[JValue](Ident(newTermName("value"))),
          instantiationMethod = ReflectionUtils.instantiationMethod[T](c, fail),
          companionObjectMaybe = companionSymbol,
          fail = fail).splice
      }
    }
  }

  def parseJValue[T : c.WeakTypeTag](c: Context)(jValue: c.Expr[JValue], instantiationMethod: c.universe.MethodSymbol,
                                                 companionObjectMaybe: Option[c.Symbol], fail: String => Nothing): c.Expr[T] = {
    import c.universe._

    val returnType = c.weakTypeOf[T]

    val decodedMethodName = instantiationMethod.name.decoded

    if (!instantiationMethod.isPublic) fail(s"'$decodedMethodName' method is not public")

    if(instantiationMethod.paramss.head.isEmpty) fail(s"'$decodedMethodName' does not take any parameters")

    val instantiationArgs = instantiationMethod.paramss.head.zipWithIndex map { case (param, index) =>
      val name = param.name.decoded
      val nameExpr = c.literal(name)
      val returnType: Type = param.typeSignatureIn(c.weakTypeOf[T])

      val readerType = appliedType(weakTypeOf[Reader[_]].typeConstructor, List(returnType))
      val reader = c.inferImplicitValue(readerType, silent = true) match {
        case EmptyTree => fail(s"an implicit reader of type $readerType must be in scope to read parameter '$name' on '$decodedMethodName' method")
        case x => x
      }

      companionObjectMaybe.filter(_ => param.asTerm.isParamWithDefault) map { companionObject =>
        val optionReader = Apply(Select(reify(DefaultReaders).tree, newTermName("OptionReader")), List(reader))
        val argValueMaybe = readValue(c)(jValue, nameExpr, optionReader)
        Apply(Select(argValueMaybe.tree, newTermName("getOrElse")), List({
          // fall back to default value for param
          val u = c.universe.asInstanceOf[Definitions with SymbolTable with StdNames]
          val getter = u.nme.defaultGetterName(u.newTermName(decodedMethodName), index + 1)
          Select(Ident(companionObject), newTermName(getter.encoded))
        }))
      } getOrElse {
        val argValue = readValue(c)(jValue, nameExpr, reader)
        argValue.tree
      }
    }

    val instantiationObject = companionObjectMaybe.filterNot(_ =>
      instantiationMethod.isConstructor
    ).map(Ident(_)).getOrElse(New(Ident(returnType.typeSymbol)))
    val instantiationCall = Select(instantiationObject, instantiationMethod.name)
    c.Expr[T](Apply(instantiationCall, instantiationArgs))
  }

  def readValue[T: c.universe.WeakTypeTag](c: Context)(jValue: c.Expr[JValue], fieldName: c.Expr[String], reader: c.Tree): c.Expr[T] = {
    import c.universe._

    val returnTypeName = c.literal(c.weakTypeOf[T].toString)
    c.weakTypeOf[T].typeSymbol.fullName
    val fieldJValue = reify {
      jValue.splice match {
        case jObject: JObject => jObject \ fieldName.splice
        case x => {
          val provided = c.Expr[Any](Ident(newTermName("x"))).splice
          throw new MappingException("Can't convert %s to %s".format(provided, returnTypeName.splice))
        }
      }
    }
    val readerRead = Select(reader, newTermName("read"))
    c.Expr[T](Apply(readerRead, List(fieldJValue.tree)))
  }
}
