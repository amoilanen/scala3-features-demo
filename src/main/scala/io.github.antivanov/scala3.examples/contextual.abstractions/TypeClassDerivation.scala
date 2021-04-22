// Adapted from the example with Eq from https://dotty.epfl.ch/docs/reference/contextual/derivation.html

import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}

trait Show[T]:
  def show(value: T): String

given Show[Int] with
  def show(value: Int): String =
    value.toString

object Show:
  def apply[T](using s: Show[T]): Show[T] =
    s

  // Works similar to toString but using square brackets instead of the round ones
  inline given derived[T](using m: Mirror.Of[T]): Show[T] =
    lazy val elemInstances = summonTypeclassInstances[m.MirroredElemTypes]
    inline m match
       case s: Mirror.SumOf[T]     => showSum(s, elemInstances)
       case p: Mirror.ProductOf[T] => showProduct(p, elemInstances)

  private def asList[T](value: T): List[Any] =
    value.asInstanceOf[Product].productIterator.toList

  inline def showSum[T](sumOf: Mirror.SumOf[T], elemTypeclasses: => List[Show[_]]): Show[T] =
    new Show[T]:
      def show(value: T): String =
        val typeName = value.getClass.getSimpleName
        val valueTypeOrdinal = sumOf.ordinal(value)
        val valueTypeclass: Show[T] = elemTypeclasses(valueTypeOrdinal).asInstanceOf[Show[T]]
        valueTypeclass.show(value)

  inline def showProduct[T](productOf: Mirror.ProductOf[T], elemTypeclasses: => List[Show[_]]): Show[T] =
    new Show[T]:
      def show(value: T): String =
        val valueClassName = value.getClass.getSimpleName
        val shownFields = asList(value).zip(elemTypeclasses).map(
          (field, showForField) =>
            showForField.asInstanceOf[Show[Any]].show(field)
        ).mkString(",")
        s"$valueClassName[$shownFields]"

  inline def summonTypeclassInstances[T <: Tuple]: List[Show[_]] =
    inline erasedValue[T] match
       case _: EmptyTuple => Nil
       case _: (t *: ts) => summonInline[Show[t]] :: summonTypeclassInstances[ts]

enum Tree[T] derives Show:
   case Branch(left: Tree[T], right: Tree[T])
   case Leaf(elem: T)

@main def TypeClassDerivation: Unit =
  import Tree._
  val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  println(Show[Tree[Int]].show(tree))