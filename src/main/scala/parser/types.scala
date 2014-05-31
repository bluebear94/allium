package parser

import scala.math.BigInt
import scala.collection.mutable.{Stack, HashMap}
import scala.util.parsing.input._
import scala.util.parsing.combinator._

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
  override def toString = s"""<$n ${a map {case (an, av) => an + "=\"" + av + "\""}}>""" +
    (if (isLone) "" else s"<$n>")
}

case class AnArray(a: Array[Type]) extends Type {
  override def toString = a.mkString(" ", "{", "}")
}

trait AbstractProc extends Type {
  def operateOn(s: Stack[Type], e: HashMap[String, Type]): Unit
}

case class Proc(z: List[Instruction]) extends AbstractProc {
  override def toString = z.mkString(" ", "[", "]")
  def operateOn(s: Stack[Type], e: HashMap[String, Type]) = {
    z foreach {(i: Instruction) => i.operateOn(s, e)}
  }
}

case class NativeProc(n: String) extends AbstractProc {
  override def toString = s"#proc.$n"
  def operateOn(s: Stack[Type], e: HashMap[String, Type]) = {
    
  }
}

trait Instruction {
  def operateOn(s: Stack[Type], e: HashMap[String, Type]): Unit
}

case class Literal(t: Type) extends Instruction {
  def operateOn(s: Stack[Type], e: HashMap[String, Type]) = s.push(t)
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
}

class Allium extends JavaTokenParsers with PackratParsers {
  lazy val intParser: PackratParser[Instruction] = wholeNumber ^^ {
    (s: String) => Literal(Integer(BigInt(s)))}
  lazy val realParser: PackratParser[Instruction] = floatingPointNumber ^^ {
    (s: String) => Literal(Real(s.toDouble))}
  lazy val stringParser: PackratParser[Instruction] = stringLiteral ^^ {
    (s: String) => Literal(AString(s))}
  
  
}

object Allium {
  val nTable = new HashMap[String, (Stack[Type], HashMap[String, Type]) => Unit]
}
