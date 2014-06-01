import parser._
import scala.util.parsing.input._
import scala.collection.mutable.{Stack, HashMap}
import java.util.Scanner

object Main {
  def main(args: Array[String]) = {
    val p = new Allium
    val s = new Stack[Type]
    val e = new HashMap[String, Type]
    var in = ""
    val sc = new Scanner(System.in)
    while (in != ":q") {
      in = sc.nextLine
      if (in != ":q") {
        val expr = s"[$in]"
        val proc = p.phrase(p.procParser)(new CharSequenceReader(expr))
        proc match {
          case p.Success(res, next) => res match {
            case a: Compound => {
              a.operateOn(s, e)
              s.pop match {
                case p: AbstractProc => p.operateOn(s, e)
                case m => println(s"?: $m")
              }
            }
            case _ => println(s"?: $res")
          }
          case _ => println(proc)
        }
        println(s.reverse.mkString(" "))
      }
    }
  }
}
