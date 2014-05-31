package parser

import scala.math.BigInt
import scala.collection.immutable.HashMap
import scala.collection.mutable.Stack
import scala.util.parsing.input._
import scala.util.parsing.combinator._

trait Type

case class Integer(n: BigInt) {
  override def toString = n.toString
}
case class Real(x: Double) {
  override def toString = x.toString
}
case class AString(s: String) {
  override def toString = s
}
case class XMLTag(n: String, a: HashMap[String, String], isLone: Boolean = false) {
  override def toString = s"""<$n ${a map {case (an, av) => an + "=\"" + av + "\""}}>""" +
    (if (isLone) "" else s"<$n>")
}

case class AnArray(a: Array[Type]) {
  override def toString = a.mkString(" ", "{", "}")
}

case class Proc(s: List[Instruction]) {
  override def toString = s.mkString(" ", "[", "]")
}

trait Instruction {
  def operateOn(s: Stack)
}

