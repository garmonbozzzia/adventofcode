package year2017

/* Created on 02.12.17 */
object Day1 {
  def solve2(s: String): Int = s.zip(s.drop(s.length/2)++s).collect{
    case (x,y) if x==y => x.toString.toInt
  }.sum

  def solve(s:String): Int = s.zip(s.tail+s.head).collect{
    case (x,y) if x==y => x.toString.toInt
  }.sum
}