package org.json4s.reflect.macros

import scala.reflect.macros.Context

/* This borrows heavily from code in Ficus: https://github.com/ceedubs/ficus */
object ReflectionUtils {
  def instantiationMethod[T : c.WeakTypeTag](c: Context, fail: String => Nothing): c.universe.MethodSymbol = {
    import c.universe._

    val returnType = c.weakTypeOf[T]

    val returnTypeTypeArgs = returnType match {
      case TypeRef(_, _, args) => args
      case _ => Nil
    }

    if (returnTypeTypeArgs.nonEmpty) fail(s"readers cannot be auto-generated for types with type parameters. Consider defining your own Reader[$returnType]")

    val companionSymbol = returnType.typeSymbol.companionSymbol match {
      case NoSymbol => None
      case x => Some(x)
    }

    val applyMethods = companionSymbol.toList.flatMap(_.typeSignatureIn(returnType).members collect {
      case m: MethodSymbol if m.name.decoded == "apply" && m.returnType <:< returnType => m
    })

    val applyMethod = applyMethods match {
      case Nil => None
      case (head :: Nil) => Some(head)
      case _ => fail(s"its companion object has multiple apply methods that return type $returnType")
    }

    applyMethod getOrElse {
      val primaryConstructor = returnType.declaration(nme.CONSTRUCTOR) match {
        case t: TermSymbol => {
          val constructors = t.alternatives collect {
            case m: MethodSymbol if m.isConstructor => m
          }
          val primaryScalaConstructor = constructors.find(m => m.isPrimaryConstructor && !m.isJava)
          primaryScalaConstructor orElse {
            if (constructors.length == 1) constructors.headOption else None
          }
        }
        case _ => None
      }
      primaryConstructor getOrElse {
        fail(s"it has no apply method in a companion object that return type $returnType, and it doesn't have a primary constructor")
      }
    }
  }

}
