package year2017

/* Created on 06.12.17 */
import org.gzb.utils.Core._
object Day6 {
  def next(input: IndexedSeq[Int]): IndexedSeq[Int] = {
    val maxIndex = input.indexOf(input.max)
    val maxValue = input(maxIndex)
    val addValue = maxValue / input.length
    val more = maxValue % input.length
    def relativeIndex(i: Int) = (i - maxIndex - 1 + input.length) % input.length
//    def newVal (at: Int) =
//      (if(at == index) 0 else input(at)) + maxValue/input.length + (at - index + 1 + input.length)%input.length
    input.zipWithIndex.map {
      case (v, i) if i == maxIndex => addValue
      case (v, i) if relativeIndex(i) < more =>
        v + addValue + 1
      case (v, i) => v + addValue
    }
  }

  // *******+++**
  // +******+++++

  def solve1(input: IndexedSeq[Int]): Int = Iterator.iterate(input -> Set.empty[IndexedSeq[Int]]){
    case (xs, cache) =>
      val nx = next(xs)
      nx -> (cache + xs)
  }.takeWhile{
    case (xs, cch) => !cch.contains(xs)
  }.length
  def solve2(input: IndexedSeq[Int]): Int = {
    val cycleStart = Iterator.iterate(input -> Set.empty[IndexedSeq[Int]]){
      case (xs, cache) =>
        val nx = next(xs)
        nx -> (cache + xs)
    }.dropWhile{
      case (xs, cch) => !cch.contains(xs)
    }.next()._1

    1 + Iterator.iterate(next(cycleStart))(next).takeWhile(_ != cycleStart).length
  }
}
