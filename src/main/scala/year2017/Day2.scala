package year2017

object Day2 extends Day {
  override type InputType = List[List[Int]]
  def solve1(input: InputType): Int = input.map(x => x.max - x.min).sum
  def solve2(input: InputType): Int = input.map(findDiv).sum
  def findDiv(xs: List[Int]): Int =
    xs.flatMap(x => findDiv(x, xs)).head
  def findDiv(v: Int, xs: List[Int]): Option[Int] =
    xs.find(x => x%v == 0 && x > v).map(_ / v)
  override def parse: String => InputType =
    _.split("\n").map(_.split("\t").map(_.toInt).toList).toList
}
