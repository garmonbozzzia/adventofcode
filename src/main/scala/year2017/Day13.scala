package year2017

/* Created on 13.12.17 */
object Day13 extends Day {
  override type InputType = Seq[(Int,Int)]

  override def parse: String => InputType = _.split("\n").map(_.split(": ") match {
    case Array(x,y) => x.toInt -> y.toInt
  }).toList
  def severity(data: InputType): Int = data.map{
    case (x,h) => if(x%(2*(h-1)) == 0) x*h else 0
  }.sum
  def solve1: InputType => Int = severity
  def solve2(firewall: InputType): Int = Stream.iterate(0)(_ + 1)
    .dropWhile(delay => firewall.exists{case (x,h) => (x+delay)%(2*h-2) == 0}).head

}
