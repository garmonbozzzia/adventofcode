package year2017

/* Created on 04.12.17 */
object Day5 {
  type Stack = (Int, IndexedSeq[Int])
  def next: Stack => Stack = {
    case (i, xs) => xs(i) + i -> xs.updated(i, xs(i) + 1)
  }
  def next2: Stack => Stack = {
    case (i, xs) if xs(i) >= 3 => xs(i) + i -> xs.updated(i, xs(i) - 1)
    case (i, xs) => xs(i) + i -> xs.updated(i, xs(i) + 1)
  }
  def isRunning: Stack => Boolean = { case(i,xs) => i >= 0 && i < xs.length }

  def solve1(input: IndexedSeq[Int]): Int = Iterator.iterate(0 -> input)(next).takeWhile(isRunning).length
  def solve2(input: IndexedSeq[Int]): Int = Iterator.iterate(0 -> input)(next2).takeWhile(isRunning).length
  //def solve2(input: Any): Any = ???
}
