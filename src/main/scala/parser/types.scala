package parser

import scala.math.BigInt
import scala.collection.mutable.{Stack, HashMap}
import scala.util.parsing.input._
import scala.util.parsing.combinator._
import util.UnescapeString

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
    Allium.nTable(n)(s, e)
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
  override def toString = s"""Compound(${z.mkString(", ", "Array(", ")")}, $isProc"""
}

case class Var(n: String) extends Instruction {
  def operateOn(s: Stack[Type], e: HashMap[String, Type]) = {
    if (n.startsWith("=") && n != "=") {
      e(n.substring(0, n.length - 1)) = s.pop
    } else if (e contains n) {
      s.push(e(n))
    } else if (Allium.nTable contains n) {
      Allium.nTable(n)(s, e)
    } else if (n startsWith "$") {
      s.push(NativeProc(n.substring(1)))
    } else throw new RuntimeException(s"$n is undefined")
  }
}

class Allium extends JavaTokenParsers with PackratParsers {
  override def skipWhitespace = false
  lazy val intParser: PackratParser[Instruction] = wholeNumber ^^ {
    (s: String) => Literal(Integer(BigInt(s)))}
  lazy val realParser: PackratParser[Instruction] = floatingPointNumber ^^ {
    (s: String) => Literal(Real(s.toDouble))}
  lazy val stringParser: PackratParser[Instruction] = stringLiteral ^^ {
    (s: String) => Literal(AString(UnescapeString.unescape(s) match {
      case Some(s) => s
      case None => throw new RuntimeException("syntax")
    }))}
  def multiple: PackratParser[List[Instruction]] = repsep(expression, " +".r)
  def arrayParser: PackratParser[Instruction] = "\\{ *".r ~ multiple ~ " *\\}".r ^^ {
    case _ ~ e ~ _ => Compound(e.toArray, false)}
  def procParser: PackratParser[Instruction] = "\\[ *".r ~ multiple ~ " *\\]".r ^^ {
    case _ ~ e ~ _ => Compound(e.toArray, true)}
  lazy val identifier: PackratParser[Instruction] = "[^\\Q[]{} \\E]*".r ^^ {
    (s: String) => Var(s)}
  def expression: PackratParser[Instruction] =
    intParser | realParser | stringParser | arrayParser | procParser | identifier
}

object Allium {
  val nTable: HashMap[String, (Stack[Type], HashMap[String, Type]) => Unit] = HashMap(
    ("+", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e0 = s.pop
      val e1 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), Integer(b)) => Integer(a + b)
        case (Integer(a), Real(b)) => Real(a.toDouble + b)
        case (Real(a), Integer(b)) => Real(a + b.toDouble)
        case (Real(a), Real(b)) => Real(a + b)
        case (AString(a), b) => AString(a + b.toString)
        case (a, AString(b)) => AString(a.toString + b)
      })
    }),
    ("-", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), Integer(b)) => Integer(a - b)
        case (Integer(a), Real(b)) => Real(a.toDouble - b)
        case (Real(a), Integer(b)) => Real(a - b.toDouble)
        case (Real(a), Real(b)) => Real(a - b)
      })
    }),
    ("!", (s: Stack[Type], e: HashMap[String, Type]) => {
      val f = s.pop
      f match {
        case g: AbstractProc => g.operateOn(s, e)
        case _ => throw new RuntimeException(s"$f is not a procedure")
      }
    })
    )
}
