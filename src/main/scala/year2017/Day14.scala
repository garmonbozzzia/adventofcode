package year2017

import year2017.Day10.hash

/* Created on 14.12.17 */
object Day14 {
  type BitField = List[String]
  type BitPoints = List[(Int,Int)]
  def removeGroup(bf: BitPoints): BitPoints =
    Iterator.iterate((List(bf.head), List.empty[(Int,Int)], bf.tail )){
      case (toCheck, group, rest) =>
        val checked = toCheck ++ group
        val newRest = rest.filterNot(toCheck.contains(_))
        val expand = toCheck.flatMap {case(x,y) => List((x+1,y), (x-1,y), (x,y+1), (x,y-1))}
          .distinct.filterNot(checked.contains(_)).filter(rest.contains(_))
        (expand, checked, newRest)
    }//.dropWhile(_._1.nonEmpty).next().traceWith{case(x,y,z) => (x.size,y.size,z.size)}._3.traceWith(_.size)
      .dropWhile(_._1.nonEmpty).next()._3
  case class BitFieldF(key: String) {
    def bits(s:String): String = s.flatMap(c => (16 + Integer.parseInt(c.toString, 16)).toBinaryString.substring(1))
    lazy val bitField: BitField = (0 to 127).map(x => s"$key-$x").map(hash).map(bits).toList
    lazy val bitPoints: BitPoints = bitField.zipWithIndex.flatMap{
      case(x, n) => x.zipWithIndex.collect{ case ('1', y) => y -> n }
    }
    lazy val answer1: Int = bitField.map(_.count(_ == '1')).sum
    lazy val answer2: Int = Iterator.iterate(bitPoints)(removeGroup).takeWhile(_.nonEmpty).size
  }

}
