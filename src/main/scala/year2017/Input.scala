package year2017

import ammonite.ops._
import org.gzb.utils.Core._
object Input {
  def day(n: Int) = read(pwd/'input/"2017"/n.toString)
  lazy val day10: List[Int] = day(10).replace("\n","").split(",").map(_.toInt).toList
  lazy val day9: String = day(9)
  lazy val day7: Array[String] = day(7).split("\n")
  lazy val day6: Vector[Int] = day(6).split("\t").map(_.toInt).toVector
  lazy val day5: Vector[Int] = day(5).split("\n").map(_.toInt).toVector
  lazy val day4: List[List[String]] = Day4.parse(day(4))//read(pwd/'input/"2017"/"4").split("\n").toList.map(_.split(" ").toList)
  lazy val day3 = 325489
  lazy val day2: List[List[Int]] = Day2.parse(day(2))
  lazy val day1: String = day(1)
}
