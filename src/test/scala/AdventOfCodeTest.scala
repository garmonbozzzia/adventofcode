/* Created on 02.12.17 */

import ammonite.ops._
import org.gzb.utils.Core._
import utest._
import year2017.Input

object TestUtils {
  def assertPair[T,S](f: T => S): ((T, S)) => Unit = {
    case (x, right) =>
      val left = f(x)
      assert(left == right)
  }

}

object AdventOfCodeTest extends TestSuite {
  import TestUtils._
  val tests = Tests {
    'Day1 - {
      import year2017.Day1._
      "Part1" - {
        List("1122" -> 3, "1111" -> 4, "1234" -> 0, "91212129" -> 9).foreach(assertPair(solve))
        solve(Input.day(1))
      }
      "Part2" - {
        List("1212" -> 6, "1221" -> 0, "123425" -> 4, "123123" -> 12, "12131415" -> 4).foreach(assertPair(solve2))
        solve2(Input.day(1))
      }
    }
    'Day2 - {
      import year2017.Day2._
      "Part1" - {
        assert(solve1(List(List(5, 1, 9, 5),List(7, 5, 3),List(2, 4, 6, 8))) == 18)
        solve1(Input.day2) <| assertEq(39126)
      }
      "Part2" - {
        assert(
          findDiv(2, List(5, 9, 2, 8)).contains(4),
          findDiv(List(5, 9, 2, 8)) == 4,
          findDiv(List(9, 4, 7, 3)) == 3,
          findDiv(List(3, 8, 6, 5)) == 2,
          solve2(List(List(5, 9, 2, 8), List(9, 4, 7, 3), List(3, 8, 6, 5))) == 9
        )
        solve2(Input.day2)
      }
    }
    'Day3 - {
      import year2017.Day3._
      "Part1" - {
        assert(
          ringNumber(1) == 0,
          ringNumber(2) == 1,
          ringNumber(9) == 1,
          ringNumber(10) == 2,
          ringNumber(24) == 2,
          ringNumber(25) == 2,
          ringNumber(26) == 3,
          toAxis(1) == 0,
          toAxis(11) == 0,
          toAxis(15) == 0,
          solve1(1) == 0,
          solve1(12) == 3,
          toAxis(23) == 0,
          solve1(23) == 2,
          solve1(1024) == 31
        )
        solve1(Input.day3)
      }
      "Part2" - {
        assert(
          next((0, 0)) == (1, 0),
          Iterator.iterate((0,0))(next).take(10).toList.last == (2,-1),
          Iterator.iterate((0,0))(next).take(23).toList.last == (0,-2),
          solve1_2(1024) == 31,
          solve1_2(325489) == solve1(325489),
          solve2(2) == 4,
          solve2(59) == 122,
          solve2(747) == 806
        )
        solve2(Input.day3)
      }
    }
    'Day4 - {
      import year2017.Day4._
      "Part1" - {
        assert(
          isValid1("aa bb cc dd ee".split(" ").toList),
          !isValid1("aa bb cc dd aa".split(" ").toList),
          isValid1("aa bb cc dd aaa".split(" ").toList))
        solve1(Input.day4)
      }
      'Part2 - {
        assert(
          isValid2("abcde fghij".split(" ").toList),
          !isValid2("abcde xyz ecdab".split(" ").toList),
          isValid2("a ab abc abd abf abj".split(" ").toList),
          isValid2("iiii oiii ooii oooi oooo".split(" ").toList),
          !isValid2("oiii ioii iioi iiio".split(" ").toList))
        solve2(Input.day4)
      }
    }
    'Day5 - {
      import year2017.Day5._
      'Part1 - {
        assert(
          next(0 -> Vector(0, 3, 0, 1, -3)) == 0 -> Vector(1, 3, 0, 1, -3),
          next(0 -> Vector(1, 3, 0, 1, -3)) == 1 -> Vector(2, 3, 0, 1, -3),
          next(1 -> Vector(2, 3, 0, 1, -3)) == 4 -> Vector(2, 4, 0, 1, -3),
          next(4 -> Vector(2, 4, 0, 1, -3)) == 1 -> Vector(2, 4, 0, 1, -2),
          next(1 -> Vector(2, 4, 0, 1, -2)) == 5 -> Vector(2, 5, 0, 1, -2),
          solve1(Vector(0, 3, 0, 1, -3)) == 5
        )
        solve1(Input.day5)
      }
      "Part2" - {
        assert(solve2(Vector(0, 3, 0, 1, -3)) == 10)
        solve2(Input.day5)
      }
    }
    'Day6 - {
      import year2017.Day6._
      'Part1 - {solve1(Input.day6) <| assertEq(7864)}
      'Part2 - {solve2(Input.day6) <| assertEq(1695)}
    }
    'Day7 - {
      'Part1 - {
        //ImportInput.save(7, "2017")
        val a = Input.day7.map(_.split(" -> ")).collect{
          case Array(k,v) => k.takeWhile(_ != ' ') -> v.split(", ").toList
        }.toList
        a.map(_._1).diff(a.flatMap(_._2).distinct).head
      }
      'Part2 - {
        val weights = Input.day7.map(_.split(' ')).map(x => x(0) -> x(1).tail.init.toInt).toMap
        val tree = Input.day7.map(_.split(" -> ")).collect{
          case Array(k,v) => k.takeWhile(_ != ' ') -> v.split(", ").toList
        }.toMap
        val root = tree.keys.toList.diff(tree.values.flatten.toList.distinct).head
        def balanceValue(node: String): Int =
          tree.get(node).fold(weights(node))(weights(node) + _.map(balanceValue).sum )
        val balanced: Map[String, Int] = weights.map{case (k,_) => k -> balanceValue(k)}
        def allDescendants(node: String): List[String] =
          tree.get(node).fold(List.empty[String]){x => x ++ x.flatMap(allDescendants)}

        tree.foldLeft(List.empty[String] -> List.empty[String]) {
          case ((bs, ubs), (n, ch)) =>
            if(ch.map(balanced).distinct.lengthCompare(1) == 0) (bs ++ allDescendants(n)) -> ubs
            else bs -> (n :: ubs)
        }._2

        allDescendants("fbtzaic")
        tree("fbtzaic").map(balanced)
        tree("mdbtyw").map(balanced)
        weights("mdbtyw")-5
      }
    }
    'Day8 - {
      import year2017.Day8._
      assert(Input.day(8).split("\n").foldLeft(Map.empty[String,Int]){process}.values.max == 5221)
      assert(Input.day(8).split("\n").scanLeft(Map.empty[String,Int]){process}.drop(1).map(_.values.max).max == 7491)
    }
    'Day9 - {
      import year2017.Day9._
      'Part1 - {
//        val garbData = List(
//          """<>""",
//          """<random characters>""",
//          """<<<<>""",
//          """<{!>}>""",
//          """<!!>""",
//          """<!!!>>""",
//          """<{o"i!a,<{i<a>""")
        assert(
          P1.groups(1).parse("""{}""").get.value == 1,
          P1.groups(1).parse("""{{{}}}""").get.value == 6,
          P1.groups(1).parse("""{{},{}}""").get.value == 5,
          P1.groups(1).parse("""{{{},{},{{}}}}""").get.value == 16,
          P1.groups(1).parse("""{<a>,<a>,<a>,<a>}""").get.value == 1,
          P1.groups(1).parse("""{{<ab>},{<ab>},{<ab>},{<ab>}}""").get.value == 9,
          P1.groups(1).parse("""{{<!!>},{<!!>},{<!!>},{<!!>}}""").get.value == 9,
          P1.groups(1).parse("""{{<a!>},{<a!>},{<a!>},{<ab>}}""").get.value == 3
        )
        P1.groups(1).parse(Input.day9).get.value <| assertEq(10050) <| assertEq(solve1(Input.day9)._1)
      }
      'Part2 - {
        assert(
          P2.garbage.parse("""<>""").get.value == 0,
          P2.garbage.parse("""<random characters>""").get.value == 17,
          P2.garbage.parse("""<<<<>""").get.value == 3,
          P2.garbage.parse("""<{!>}>""").get.value == 2,
          P2.garbage.parse("""<!!>""").get.value == 0,
          P2.garbage.parse("""<!!!>>""").get.value == 0,
          P2.garbage.parse("""<{o"i!a,<{i<a>""").get.value == 10
        )
        P2.groups.parse(Input.day9).get.value <| assertEq(4482) <| assertEq(solve2(Input.day9)._1)
        P2.groups.parse(Input.day9).get.value ==> 4482// <| assertEq(solve2(Input.day9)._1)
      }
    }
    'Day10 - {
      //ImportInput.save(10,"2017")
      Input.day10
      import year2017.Day10._
      assert(
        solve1(List(3, 4, 1, 5),5) == 12,
        solve1(Input.day10,256) == 6952,
        "1,2,3".map(_.toInt).mkString(",") == "49,44,50,44,51",
        lengths("1,2,3") == List(49,44,50,44,51,17,31,73,47,23),
        List(65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22).reduce(_ ^ _) == 64,
        List(64,7,255).map(x => if(x<16) s"0${x.toHexString}" else x.toHexString).mkString == "4007ff",
        List(64,7,255).map(x => (x+256).toHexString.substring(1)).mkString == "4007ff",
        hash("") == "a2582a3a0e66e6e86e3812dcb672a272",
        hash("AoC 2017") == "33efeb34ea91902bb2f59c9920caa6cd",
        hash("1,2,3") == "3efbe78a8d82f29979031a4aa0b16a9d",
        hash("1,2,4") == "63960835bcdc130f0b66d7ff4f6a5a8e"
      )
      hash(Input.day10.mkString(",")) <== "28e7c4360520718a5dc811d3942cf1fd"
      hash(Input.day10.mkString(",")) <== "28e7c4360520718a5dc811d3942cf1fd"
    }
    'Day11 - {
      //ImportInput.save(11,"2017")
      import year2017.Day11._
      'Part1 - assert(
          solve1("ne,ne,ne") == 3,
          solve1("ne,ne,sw,sw") == 0,
          solve1("ne,ne,s,s") == 2,
          solve1("se,sw,se,sw,sw") == 3
        ); solve1(Input.day(11)) <== 707

      'Part2 - (solve2(Input.day(11).split(",")) <== 1490)
    }
    'Day12 - {
      import year2017.Day12._
      val testInputString = """0 <-> 2
                        |1 <-> 1
                        |2 <-> 0, 3, 4
                        |3 <-> 2, 4
                        |4 <-> 2, 3, 6
                        |5 <-> 6
                        |6 <-> 4, 5""".stripMargin
      (testInputString |> parse |> solve).size <== 6
      (testInputString |> parse |> solve2) <== 2
      (Input.day(12) |> parse |> solve).size <== 130
      (Input.day(12) |> parse |> solve2) <== 189
    }
    'Day13 - {
      //ImportInput.save(13,"2017")
      import year2017.Day13._
      val testInput = "0: 3\n1: 2\n4: 4\n6: 4"
      (testInput |> parse |> solve1) ==> 24
      (Input.day(13) |> parse |> solve1) ==> 2160
      (testInput |> parse |> solve2) ==> 10
      (Input.day(13) |> parse |> solve2) <== 3907470
    }
    'Day14 - {
      import year2017.Day14._
      //val testBitField = BitFieldF("flqrgnkx").bitField.map(_.take(8)).take(8)//.traceWith(_.map(_.mkString).mkString("\n"))
      val bf1 = BitFieldF("flqrgnkx")
      val bf2 = BitFieldF("nbysizxe")
      'Part1 - {
        bf1.answer1 <== 8108
        bf2.answer1 <== 8216
      }
      'Part2 - {
        bf1.answer2 <== 1242
        bf2.answer2 <== 1139
      }
    }
  }
}