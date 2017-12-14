package year2017

import scala.collection.immutable

/* Created on 12.12.17 */
object Day11 extends Day {
  override type InputType = Seq[String]
  override def parse: String => Seq[String] = ???
  def coord: List[Int] => (Int,Int) = {
    case List(a,b,c,d,e,f) => (a-c-d+f) -> (b+c-e-f)
  }
  import org.gzb.utils.Core._
  def coord2(p: List[Int]) = (0 to 5).map(i =>
    p.splitAt(i).swap.reduce(_++_)).map(coord).toList
  def solve1(s: String): Int = coord2(List("n","ne","se","s","sw","nw")
    .map(s.split(",").groupBy(x => x).mapValues(_.length).getOrElse(_,0))
  ).collect{case (x,y) if x >=0 && y >=0 => x+y}.head
  def solve2(s: InputType): Int = s.scanLeft((0,0)){
    case ((n,ne), "n" ) => (n+1,ne)
    case ((n,ne), "ne") => (n,ne+1)
    case ((n,ne), "s" ) => (n-1, ne)
    case ((n,ne), "se") => (n-1,ne+1)
    case ((n,ne), "sw") => (n,ne-1)
    case ((n,ne), "nw") => (n+1, ne-1)
    case _ => throw new Exception
  }.map {
    case (x,y) if x >= 0 && y >= 0 => x + y
    case (x,y) if x <= 0 && y <= 0 => -(x+y)
    case (x,y) if x > 0 && y < 0 => math.max(x,-y)
    case (x,y) if x < 0 && y > 0 => math.max(-x,y)
  }.max

}
