package year2017

/* Created on 12.12.17 */
import org.gzb.utils.Core._
object Day10 {
  def suffix = List(17,31,73,47,23)
  def lengths(input: String): List[Int] = input.map(_.toInt).toList ++ suffix
  case class State(currentPosition: Int, skipSize: Int, state: List[Int]) {
    def zeroPoint: Int = if(currentPosition == 0) 0 else state.length - currentPosition
    def next(length: Int): State = {
      val shift = (length + skipSize) % state.length
      State(
        (currentPosition + length + skipSize) % state.length,
        (skipSize + 1) % state.length,
        state.patch(0, state.take(length).reverse, length)
          .splitAt(shift).swap.reduce(_ ++ _)
      )
    }
    def normalized: List[Int] = state.splitAt(zeroPoint).swap.reduce(_ ++ _)
    def dh: String = denseHash(normalized)
    def solve1: Int = normalized.head*normalized(1)
  }
  def hash(input: String): String = Iterator.iterate(State(0,0,(0 until 256).toList))(x => round(lengths(input),x))
    .drop(64)
    .next().dh
  def round(xs: List[Int], s: State): State = xs.foldLeft(s)(_ next _)
  def solve1(xs: List[Int], n: Int): Int = round(xs, State(0,0,(0 until n).toList)).solve1
  def denseHash(xs: List[Int]): String = xs.grouped(16).map(_.reduce(_ ^ _))
    .map(x => (x+256).toHexString.substring(1))
    .mkString
}
