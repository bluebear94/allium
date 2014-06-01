package parser

import scala.math._
import scala.collection.mutable.{ Stack, HashMap }
import scala.util.parsing.input._
import scala.util.parsing.combinator._
import util.UnescapeString
import java.util.Arrays

class Allium extends JavaTokenParsers with PackratParsers {
  override def skipWhitespace = false
  lazy val intParser: PackratParser[Instruction] = wholeNumber ^^ {
    (s: String) => Literal(Integer(BigInt(s)))
  }
  lazy val realParser: PackratParser[Instruction] = floatingPointNumber ^^ {
    (s: String) => Literal(Real(s.toDouble))
  }
  lazy val stringParser: PackratParser[Instruction] = stringLiteral ^^ {
    (s: String) =>
      Literal(AString(UnescapeString.unescape(s) match {
        case Some(s) => s
        case None => throw new RuntimeException("syntax")
      }))
  }
  def multiple: PackratParser[List[Instruction]] = repsep(expression, " +".r)
  def arrayParser: PackratParser[Instruction] = "\\{ *".r ~ multiple ~ " *\\}".r ^^ {
    case _ ~ e ~ _ => Compound(e.toArray, false)
  }
  def procParser: PackratParser[Instruction] = "\\[ *".r ~ multiple ~ " *\\]".r ^^ {
    case _ ~ e ~ _ => Compound(e.toArray, true)
  }
  lazy val identifier: PackratParser[Instruction] = "[^\\Q[]{} \\E]*".r ^^ {
    (s: String) => Var(s)
  }
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
    }),
    ("*", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), Integer(b)) => Integer(a * b)
        case (Integer(a), Real(b)) => Real(a.toDouble * b)
        case (Real(a), Integer(b)) => Real(a * b.toDouble)
        case (Real(a), Real(b)) => Real(a * b)
      })
    }),
    ("/", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), Integer(b)) => Real(a.toDouble / b.toDouble)
        case (Integer(a), Real(b)) => Real(a.toDouble / b)
        case (Real(a), Integer(b)) => Real(a / b.toDouble)
        case (Real(a), Real(b)) => Real(a / b)
      })
    }),
    ("\\", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), Integer(b)) => Integer(a / b)
        case (Integer(a), Real(b)) => Integer((a.toDouble / b).toInt)
        case (Real(a), Integer(b)) => Integer((a / b.toDouble).toInt)
        case (Real(a), Real(b)) => Integer((a / b).toInt)
      })
    }),
    ("^", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), Integer(b)) => Integer(a pow b.intValue)
        case (Integer(a), Real(b)) => Real(Math.pow(a.toDouble, b))
        case (Real(a), Integer(b)) => Real(Math.pow(a, b.toDouble))
        case (Real(a), Real(b)) => Real(Math.pow(a, b))
      })
    }),
    ("=", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push(Boolean(e0 == e1))
    }),
    ("!=", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push(Boolean(e0 != e1))
    }),
    (">", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), Integer(b)) => Boolean(a > b)
        case (Integer(a), Real(b)) => Boolean(a.toDouble > b)
        case (Real(a), Integer(b)) => Boolean(a > b.toDouble)
        case (Real(a), Real(b)) => Boolean(a > b)
        case (AString(a), AString(b)) => Boolean(a > b)
      })
    }),
    ("<", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), Integer(b)) => Boolean(a < b)
        case (Integer(a), Real(b)) => Boolean(a.toDouble < b)
        case (Real(a), Integer(b)) => Boolean(a < b.toDouble)
        case (Real(a), Real(b)) => Boolean(a < b)
        case (AString(a), AString(b)) => Boolean(a < b)
      })
    }),
    (">=", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), Integer(b)) => Boolean(a >= b)
        case (Integer(a), Real(b)) => Boolean(a.toDouble >= b)
        case (Real(a), Integer(b)) => Boolean(a >= b.toDouble)
        case (Real(a), Real(b)) => Boolean(a >= b)
        case (AString(a), AString(b)) => Boolean(a >= b)
      })
    }),
    ("<=", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), Integer(b)) => Boolean(a <= b)
        case (Integer(a), Real(b)) => Boolean(a.toDouble <= b)
        case (Real(a), Integer(b)) => Boolean(a <= b.toDouble)
        case (Real(a), Real(b)) => Boolean(a <= b)
        case (AString(a), AString(b)) => Boolean(a <= b)
      })
    }),
    ("to", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop.toString.toLowerCase
      val e0 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), "integer") => e0
        case (Integer(a), "real") => Real(a.toDouble)
        case (Integer(a), "string") => AString(a.toString)
        case (Real(a), "integer") => Integer(BigDecimal(a).toBigInt)
        case (Real(a), "real") => e0
        case (Real(a), "string") => AString(a.toString)
        case (AString(a), "integer") => Integer(BigInt(a))
        case (AString(a), "real") => Real(a.toDouble)
        case (AString(a), "string") => e0
      })
    }),
    ("dup", (s: Stack[Type], e: HashMap[String, Type]) => {
      s.push(s.top)
    }),
    ("rem", (s: Stack[Type], e: HashMap[String, Type]) => {
      s.pop()
    }),
    ("tp", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push(e1)
      s.push(e0)
    }),
    ("&&", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push(Boolean(toBool(e0) && toBool(e1)))
    }),
    ("||", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push(Boolean(toBool(e0) || toBool(e1)))
    }),
    ("^^", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push(Boolean(toBool(e0) ^ toBool(e1)))
    }),
    ("&", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), Integer(b)) => Integer(a & b)
      })
    }),
    ("|", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), Integer(b)) => Integer(a | b)
      })
    }),
    ("b^", (s: Stack[Type], e: HashMap[String, Type]) => {
      val e1 = s.pop
      val e0 = s.pop
      s.push((e0, e1) match {
        case (Integer(a), Integer(b)) => Integer(a ^ b)
      })
    })
  )
  def bti(b: Boolean) = if (b) 1 else 0
  def Boolean(b: Boolean) = Integer(bti(b))
  def toBool(t: Type) = t match {
    case Integer(n) => n != 0
    case Real(x) => x != 0
    case AString(s) => s != ""
    case AnArray(a) => !a.isEmpty
    case _ => true
  }
}
