package parser

import scala.math.BigInt
import scala.collection.mutable.{ Stack, HashMap }
import scala.util.parsing.input._
import scala.util.parsing.combinator._
import util.UnescapeString
import scala.xml._

trait Type

case class Integer(n: BigInt) extends Type {
  override def toString = n.toString
}
case class Real(x: Double) extends Type {
  override def toString = x.toString
}
case class AString(s: String) extends Type {
  override def toString = s
}
case class XMLTag(n: String, a: HashMap[String, String], isLone: Boolean = false) extends Type {
  override def toString = s"""<$n ${a map { case (an, av) => an + "=\"" + av + "\"" } mkString " "}>""" +
    (if (isLone) "" else s"</$n>")
}

case class XMLElem(xml: NodeSeq) extends Type {
  override def toString = xml.toString
}

case class AnArray(a: Array[Type]) extends Type {
  override def toString = a.mkString("{", " ", "}")
  override def equals(that: Any) = that match {
    case that: AnArray => a.toList == that.a.toList
    case _ => false
  }
}

trait AbstractProc extends Type {
  def operateOn(s: Stack[Type], e: HashMap[String, Type]): Unit
}

case class Proc(z: List[Instruction]) extends AbstractProc {
  override def toString = z.map(_.toString).mkString("[", " ", "]")
  def operateOn(s: Stack[Type], e: HashMap[String, Type]) = {
    z foreach { (i: Instruction) => i.operateOn(s, e) }
  }
}

case class NativeProc(n: String) extends AbstractProc {
  override def toString = s"#proc.$n"
  def operateOn(s: Stack[Type], e: HashMap[String, Type]) = {
    Allium.nTable(n)(s, e)
  }
}

trait Instruction {
  def operateOn(s: Stack[Type], e: HashMap[String, Type]): Unit
}

case class Literal(t: Type) extends Instruction {
  def operateOn(s: Stack[Type], e: HashMap[String, Type]) = s.push(t)
  override def toString = t.toString
}

case class Compound(z: Array[Instruction], isProc: Boolean) extends Instruction {
  def operateOn(s: Stack[Type], e: HashMap[String, Type]) = {
    if (isProc) s.push(Proc(z.toList))
    else {
      var l: List[Type] = List()
      for (i <- z) {
        i.operateOn(s, e)
        l = s.pop :: l
      }
      s.push(AnArray(l.reverse.toArray))
    }
  }
  override def toString = z.mkString(if (isProc) "[" else "{", " ", if (isProc) "]" else "}")
}

case class Var(n: String) extends Instruction {
  def operateOn(s: Stack[Type], e: HashMap[String, Type]) = {
    if (n.startsWith("=") && n != "=") {
      e(n.substring(1)) = s.pop
    } else if (e contains n) {
      s.push(e(n))
    } else if (Allium.nTable contains n) {
      Allium.nTable(n)(s, e)
    } else if (n startsWith "$") {
      s.push(NativeProc(n.substring(1)))
    } else throw new RuntimeException(s"$n is undefined")
  }
}

