/* Created on 05.12.17 */
import ammonite.ops._
import utest._
import org.gzb.utils.Core.Traceable


object Day19_2 {
  def replace(str: String): String = str
    .replace("Ar", ")")
    .replace("Al", "A")
    .replace("Ca", "D")
    .replace("Mg", "G")
    .replace("Si", "S")
    .replace("Rn", "(")
    .replace("Th", "U")
    .replace("Ti", "T")
    .replace("Y", "|")
    .replace("e",  "E").trace
  //.traceWith(x => x == x.toUpperCase)
  def parse(input: String): (String, Seq[(String, String)]) = {
    val data = input.split("\n").filter(_.nonEmpty)
    val a: Seq[(String, String)] = data.init.map(_.split(" => ") match {
      case Array(x,y) => replace(x) -> replace(y)
    }).toList
    data.last -> a
  }
}

case class Primes(cache: Seq[Int] = Seq.empty) {
  def isPrime(n: Int): Boolean = {
    val sqrt = math.sqrt(n).toInt
    cache.takeWhile(_ < sqrt).exists(n % _ == 0)
  }
  def primes: List[Int] =  1 :: List(2,3,5)//Stream.iterate(2)(_ + 2)
}

object Y15D20 {
  val primes: Stream[Int] = List(2,3,5,7,11,13,17,19).toStream

  def primeDivisors(n: Int): List[Int] = {
    val sqrt = math.sqrt(n).toInt
    primes.takeWhile(_ <= sqrt).find(n % _ == 0).fold(List(1,n)) {
      k =>
        val ds = primeDivisors(n / k)
        (ds ++ ds.map(_ * k).takeWhile(_ <= n)).distinct.sorted
    }
  }
}

object Year2015 extends TestSuite {
//object Year2015 {
  val tests = Tests {

    'Day20 - {
      import Y15D20._
      primeDivisors(119).sum
    }

    'Day19p2 - {
      import Day19_2._
      val input = replace(read(pwd / 'input / "2015" / "19"))
      assert(input == input.toUpperCase)
      val (end, tr) = parse(input)
      val itr: Seq[(String, String)] = tr.map(_.swap)
      //end.replace("Z", "Z\n")
      end.groupBy(identity).mapValues(_.length).mkString("\n")
      end.groupBy(identity).mapValues(_.length).values.sum
      val a = tr.map(x => x._1 + x._2).distinct
      val r1 = end.count(_ == '|')
      val r2 = end.count(_ == '(') - r1
      val r0  = end.length - r1*5 - r2*3 - 1
      r0 + r1 + r2
    }

    'Day19 - {
      import year2015.Day19._
      val a = List("e"->"O", "O"->"HH", "H"->"OH")
        .map{case(x,y) => s"$x => $y"}.mkString("\n") + "\n\nHOH"
      val b = List("e"->"O", "e"->"H", "O"->"HH", "H"->"OH", "H"->"HO")
        .map{case(x,y) => s"$x => $y"}.mkString("\n") + "\n\nHOHOHO"

      val input = read(pwd / 'input / "2015" / "19")
      'Part1 - {
        assert(solve1(input) == 576)
        assert(next("HOH", List("H" -> "HO", "H" -> "OH", "O" -> "HH")).lengthCompare(4) == 0)
        assert(next("HOHOHO", List("H" -> "HO", "H" -> "OH", "O" -> "HH")).lengthCompare(7) == 0)
        assert(next("D", List("H" -> "HO", "H" -> "OH", "O" -> "HH")).isEmpty)
      }
      'Part2 - {
        //assert(prev("e", "HOH", reverse(List("e"->"O", "O"->"HH", "H"->"OH"))) == 3)
        parse(a).trace
        prev("e", "HOH", reverse(List("e"->"O", "O"->"HH", "H"->"OH")))
        solve2(a.trace)
        solve2(b.trace)
        //assert(prev("e", "HOHOHO", reverse(List("e"->"O", "e"->"H", "O"->"HH", "H"->"OH", "H"->"HO"))) == 6)
        //solve2(input)
      }
    }
  }
}