package router

import scala.util.parsing.combinator._
import scala.util.matching.Regex

import scala.annotation.tailrec

case class Capture(val regexp: String, val captureName: Option[String])

object PathParser extends RegexParsers {
  def route = rep(term)

  def term = (namedRegexCapture | namedCapture | wildcard | normalString)

  def namedRegexCapture = "{" ~> """(?:\{[0-9,]+\}|[^{}]+)+""".r <~ "}" ^^ { s => 
    val e = s.split(":",2)
    val re = if (e.size == 2) "(" + e(1) + ")" else """([^/]+)"""
    if (e.size == 2 && isNormalCapture(e(1)))
      throw new Exception("You can't include parens in your custom rule.")
    Capture(re, Some(e(0)))
  }
  def namedCapture = ":" ~> """[A-Za-z0-9_]+""".r ^^ { s => Capture("""([^/]+)""", Some(s)) }
  def wildcard = """\*""".r                     ^^ { s => Capture("""(.+)""", Some("*")) }
  def normalString = """[^{:*]+""".r            ^^ { s => Capture(quotemeta(s), None) }

  def apply(input: String): Either[String, List[Capture]] = parseAll(route, input) match {
    case Success(capture, next)        => Right(capture)
    case NoSuccess(errorMessage, next) =>
      Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
  }

  private def quotemeta(str: String): String = {
    str.replaceAll("""([\.\\\+\*\?\[\^\]\$\(\)\/])""", """\\$1""")
  }

  private def isNormalCapture(c: String): Boolean = {
    """\((?!\?:)""".r.findFirstIn( c ).nonEmpty
  }
}

class Route(val captureList: List[Capture], val dest: String) {
  val regexp = new Regex( """^""" + captureList.map( c => c.regexp ).reduce(_ + _) + """$""" )
  val keys = captureList.map( _.captureName ).flatten
}

class Router() {
  val routes = new scala.collection.mutable.ListBuffer[Route]()

  def add(route: String, dest: String): Unit = {
    val captured = PathParser(route)

    captured match {
      case Left(e)  => throw new Exception(e.toString)
      case Right(e) => routes += new Route(e, dest)
    }
  }

  def matchRoutes(path: String): Option[(String, Map[String, String])] = {
    @tailrec
    def innerMatchRoutes(rs: List[Route]): Option[(String, Map[String, String])] = {
      if (rs.size == 0)
        None
      else {
        val route = rs.head
        val m = route.regexp.findFirstMatchIn(path)
        m match {
          case Some(v) => {
            val result = scala.collection.mutable.Map[String, String]()
            route.keys.zipWithIndex.foreach( k => result += (k._1 -> v.group(k._2 + 1)))
            Some((route.dest, result.toMap))
          }
          case None    => innerMatchRoutes(rs.tail)
        }
      }
    }

    innerMatchRoutes(routes.toList)
  }
}
