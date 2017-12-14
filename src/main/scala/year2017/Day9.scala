package year2017

import fastparse.all.{CharPred, P, Parser}
import fastparse.core

/* Created on 12.12.17 */
object Day9 {
  import fastparse.all._
  object P1 {
    private val ignored1 = P("!" ~ AnyChar)
    private val ignored2 = CharPred(_ != '>')
    private val garbage = P("<" ~ (ignored1 | ignored2).rep ~ ">").map(_ => 0)

    def groups(i: Int): Parser[Int] =
      P("{" ~ (garbage | groups(i + 1)).rep(sep = ",") ~ "}").map(i + _.sum)
  }
  object P2 {
    private val ignored1 = P("!" ~ AnyChar).map(_ => 0)
    private val ignored2 = CharPred(_ != '>').map(_ => 1)
    val garbage: core.Parser[Int, Char, String] = P("<" ~ (ignored1 | ignored2).rep ~ ">").map(_.sum)

    def groups: Parser[Int] =
      P("{" ~ (garbage | groups).rep(sep = ",") ~ "}").map(_.sum)

  }
  def solve1(input: String): (Int, Int, Boolean, Boolean) = input.foldLeft((0,1,false,false)) {
    case ((s, l, false, _), '{') => (s + l, l + 1, false, false)
    case ((s, l, false, _), '}') => (s, l - 1, false, false)
    case ((s, l, false, _), '<') => (s, l, true, false)
    case ((s, l, true, false), '>') => (s, l, false, false)
    case ((s, l, true, false), '!') => (s, l, true, true)
    case ((s, l, true, false), _) => (s, l, true, false)
    case ((s, l, _, true), _) => (s, l, true, false)
    case x => x._1
  }
  def solve2(input: String): (Int, Boolean, Boolean) = input.foldLeft((0,false,false)) {
    case ((s, false, _), '<') => (s, true, false)
    case ((s, true, false), '>') => (s, false, false)
    case ((s, true, false), '!') => (s, true, true)
    case ((s, _, true), _) => (s, true, false)
    case ((s, true, false), _) => (s+1, true, false)
    case x => x._1
  }
}
