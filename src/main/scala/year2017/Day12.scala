package year2017

/* Created on 12.12.17 */
abstract class Day {
  type InputType
  def parse: String => InputType
}
object Day12 extends Day{
  import ammonite.ops._
  type InputType = Map[Int, List[Int]]
  override def parse: String => InputType = _.split("\n").map(_.split(" <-> ") match {
    case Array(a, b) => a.toInt -> b.split(", ").map(_.toInt).toList
  }).toMap
  def iterate(res: Set[Int], n: Int): InputType => Set[Int] = parsed =>
    if(res.contains(n)) res else parsed(n).foldLeft(res + n)(iterate(_,_)(parsed))
  def solve(parsed: InputType): Set[Int] = iterate(Set.empty, parsed.keys.head)(parsed)
  def solve2(parsed: InputType): Int = Iterator.iterate(parsed)(x =>
    solve(x) |> (ex => x.filterKeys(!ex.contains(_)))
  ).takeWhile(_.nonEmpty).size
}
