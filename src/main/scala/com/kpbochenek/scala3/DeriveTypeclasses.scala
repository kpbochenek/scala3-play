package com.kpbochenek.scala3

import BinaryTree._

import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

// Pierwsza typeclass'a dla której ręcznie zdefiniujemy instancję dla BinaryTree
trait Eq[A]:
  def eq(x: A, y: A): Boolean


// Druga typeclass'a dla której wygenerujemy instancję przez kompilator dla BinaryTree
trait Show[A]:
  def show(x: A): String

def showProduct[T](shows: => List[Show[_]]): Show[T] =
  new Show[T] {
    def show(x: T): String = {
      (x.asInstanceOf[Product].productIterator).zip(shows.iterator).map {
        case (p, s) => s.asInstanceOf[Show[Any]].show(p)
      }.mkString(s"${x.getClass.getSimpleName}(", ",", ")")
    }
  }

def showSum[T](s: Mirror.SumOf[T], shows: => List[Show[_]]): Show[T] =
  new Show[T] {
    def show(x: T): String = {
      val index = s.ordinal(x)
      shows(index).asInstanceOf[Show[T]].show(x)
    }
  }

inline def summonAll[T <: Tuple]: List[Show[_]] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[Show[t]] :: summonAll[ts]

object Show:
  inline given derived[T](using m: Mirror.Of[T]): Show[T] =
    lazy val shows = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T] => showSum(s, shows)
      case _: Mirror.ProductOf[T] => showProduct(shows)

given Show[Int] with
  def show(x: Int): String = x.toString

given Eq[Int] with
  def eq(a: Int, b: Int): Boolean = a == b



enum BinaryTree[+A] derives Show:
  case Node(value: A, left: BinaryTree[A], right: BinaryTree[A])
  case Leaf() // jak to jest Leaf (obiekt) to wtedy nie działa getSimpleName



given[A](using ev: Eq[A]): Eq[BinaryTree[A]] with
  def eq(x: BinaryTree[A], y: BinaryTree[A]): Boolean = (x, y) match {
    case (Leaf(), Leaf()) => true
    case (Node(v1, l1, r1), Node(v2, l2, r2)) => ev.eq(v1,v2) && eq(l1, l2) && eq(r1, r2)
    case _ => false
  }

object DeriveTypeclasses:
  def cmp(a: BinaryTree[Int], b: BinaryTree[Int])(using eq: Eq[BinaryTree[Int]]): Boolean = eq.eq(a, b)

  def main(args: Array[String]): Unit =
    println("Yay working derive")
    val a = Node(3, Node(5, Leaf(), Leaf()), Node(5, Leaf(), Leaf()))
    val b = Node(3, Node(1, Leaf(), Leaf()), Node(2, Leaf(), Leaf()))
    println(a)
    println(b)
    println(cmp(a, b))
    println(implicitly[Eq[BinaryTree[Int]]].eq(b, b))

    println(implicitly[Show[BinaryTree[Int]]].show(a))
    println(implicitly[Show[BinaryTree[Int]]].show(b))

