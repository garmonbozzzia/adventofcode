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
  import fastparse.all._
  val number = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )

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
    'Day15 - {
      //ImportInput.save(15)
      val (mult1, mult2)= (BigInt(16807), BigInt(48271))
      val (key1,key2) = (BigInt(116), BigInt(299))
      val div = 2147483647
      val div16 = Iterator.iterate(1)(_*2).drop(16).next()
      def gen(key: BigInt, mult: BigInt) = Stream.iterate(key)(x => (x * mult) % div)
      //def gen(key: BigInt, mult: BigInt) = Stream.iterate(key)(x => (x * mult) % div)
      //(gen(key1,mult1) zip gen(key2, mult2)).take(40000000).count{case(x,y) => (x % div16) == (y % div16) }.trace
//      (gen(key1,mult1) zip gen(key2, mult2)).take(5000000).count{case(x,y) => (x % div16) == (y % div16) }.trace ==> 309
      (gen(65,mult1) zip gen(8921, mult2)).take(5).count{case(x,y) => (x % div16) == (y % div16) } ==> 1
      (gen(65,mult1).filter(_ % 4 == 0) zip gen(8921, mult2).filter(_ % 8 == 0))
        .take(1056).count{case(x,y) => ((x % div16)== (y % div16))} ==> 1
      (gen(65,mult1).filter(_ % 4 == 0) zip gen(8921, mult2).filter(_ % 8 == 0))
        .take(1055).count{case(x,y) => ((x % div16)== (y % div16))} ==> 0
      (gen(65,mult1).filter(_ % 4 == 0) zip gen(8921, mult2).filter(_ % 8 == 0))
        .take(5000000).count{case(x,y) => ((x % div16)== (y % div16))}.trace ==> 309
      (gen(key1,mult1).filter(_ % 4 == 0) zip gen(key2, mult2).filter(_ % 8 == 0)).take(5000000)
        .filter{case(x,y) => (x % div16) == (y % div16) }.map(_ <| (_ => print("+"))).size
    }
    'Day16 - {
      //ImportInput.save(16)
      sealed trait DanceMove
      final case class Spin(n: Int) extends DanceMove
      final case class Exchange(n: Int, m: Int) extends DanceMove
      final case class Partner(a: String, b: String) extends DanceMove
      val s = P("s" ~ number).map(Spin(_))
      val x = P("x" ~ number ~"/" ~ number).map(_.reduce( Exchange(_,_)))
      val p = P("p" ~ AnyChar.! ~ "/" ~ AnyChar.!).map(_.reduce(Partner(_,_)))

      def parse(input: String) = P((s|x|p).rep(sep = ",")~End).parse(input).get.value
      def solve(moves: Seq[DanceMove], init: String) = moves
//          .traceWith(_.length)
//          .traceWith(_.mkString("\n"))
          //.traceWith(_.take(100).mkString("\n"))
//          .traceWith(_.collect {case s: Spin => s } mkString("\n"))
//          .traceWith(_.collect {case s: Exchange => s } mkString("\n"))
//          .traceWith(_.collect {case s: Partner => s } mkString("\n"))
  //          .traceWith(_.collect {case s: Spin => s } size)
  //          .traceWith(_.collect {case s: Exchange => s } size)
  //          .traceWith(_.collect {case s: Partner => s } size)
        .foldLeft(init){
        case (res,Spin(s)) => res.splitAt(res.length - s).swap.reduce(_+_)
        case (res,Exchange(n, m)) =>
          val sw = res(n)
          res.patch(n,List(res(m)),1).patch(m,List(res(n)),1)
        case (res,Partner(a, b)) => res.replace(a,"*").replace(b,a).replace("*",b)
      }

      solve(parse("s1,x3/4,pe/b"), "abcde") ==> "baedc"
      solve(parse(Input.day(16)), "abcdefghijklmnop") ==> "giadhmkpcnbfjelo"
      ("abcdefghijklmnop" zip "giadhmkpcnbfjelo") mkString (", ")
      val moves = parse(Input.day(16))
      Stream.iterate("abcdefghijklmnop")(solve(moves,_)).take(50).mkString("\n").trace
//      abcdefghijklmnop
//      giadhmkpcnbfjelo
//      nihjpcmfgaekblod
//      ehlkjingcdampfob
//      aemgilbncophfjkd
//      kbnacipheglojmfd
//      cjhpbikanomdeglf
//      imfkcnjbgoeldahp
//      geincmljkphdofab
//      ipcomlhnjagdbkfe
//      icldjfpengkaobmh
//      jicgbkflanmhoedp
//      kbcdpmgfienlhaoj
//      gikdfhnjcabmeplo
//      aifejchmgkpnblod
//      pflneiagcdkhjmob
//      kphgilbacojfmend
//      nbakcijfpgloehmd
//      cefjbinkaohdpglm
//      ihmncaebgopldkfj
//      gpiachlenjfdomkb
//      ijcohlfaekgdbnmp
//      icldemjpagnkobhf
//      eicgbnmlkahfopdj
//      nbcdjhgmipalfkoe
//      gindmfaeckbhpjlo
//      kimpecfhgnjablod
//      jmlapikgcdnfehob
//      njfgilbkcoemhpad
//      abknciemjglopfhd
//      cpmebiankofdjglh
//      ifhackpbgojldnme
//      gjikcflpaemdohnb
//      iecoflmkpngdbahj
//      icldphejkganobfm
//      picgbahlnkfmojde
//      abcdefghijklmnop
//      giadhmkpcnbfjelo
//      nihjpcmfgaekblod
//      ehlkjingcdampfob
//      aemgilbncophfjkd
//      kbnacipheglojmfd
//      cjhpbikanomdeglf
//      imfkcnjbgoeldahp
//      geincmljkphdofab
//      ipcomlhnjagdbkfe
//      icldjfpengkaobmh
//      jicgbkflanmhoedp
//      kbcdpmgfienlhaoj
//      gikdfhnjcabmeplo

      // (a,g),
      // (g,k),
      // (k,b),
      // (b,i),
      // (i,c),
      // (c,a),

      // (d,d),

      // (e,h),
      // (h,p),
      // (p,o)
      // (o,l),
      // (l,f),
      // (f,m),
      // (m,j),
      // (j,n),
      // (n,e),

      //      P((s|x|p).rep(sep = ",")~End).parse(Input.day(16)).get.value.foldLeft("abcdefghijklmnop"){
//        case (res,s: Int) => res.splitAt(res.length - s).swap.reduce(_+_)
//        case (res,(n: Int, m: Int)) =>
//          val sw = res(n)
//          res.patch(n,List(res(m)),1).patch(m,List(res(n)),1)
//        case (res,(a: String, b: String)) =>
//          res.replace(a,"*").replace(b,a).replace("*",b)
//      }
      //"abcde"
    }
    'Day17 - {
      //ImportInput.save(17)
      //Input.day(17)
      val steps = 394
      def answer1 = ((1 to 2017).foldLeft(List(0), 0) {
        case ((buffer, index), v) =>
          val newIndex = (index + steps) % buffer.length
          buffer.patch(newIndex + 1, List(v), 0) -> (newIndex + 1)
      }._1.dropWhile(_ != 2017) ++ List(0)).take(2).last

      def answer2 = (1 to 50000000).foldLeft(0 -> 0) {
        case ((index, afterZero), v) =>
          val newIndex = (index + steps) % v
          (newIndex + 1) -> (if (newIndex == 0) v else afterZero)
      }._2
      answer1
      assert(answer2.trace == 10150888)
    }
    'Day18 - {
      //ImportInput.save(18)
      import fastparse.all._
      type Command = Array[String]
      val testCommands = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
        .split("\n").map(_.split(" ")).toList
      val commands = Input.day(18).split("\n").map(_.split(" ")).toList

      case class State(p: Long = 0L, args: Map[String, Long] = Map.empty, sounds: List[Long] = Nil) {
        import scala.util.Try
        def valOf(v: String) = Try(v.toLong).toOption.getOrElse(args.getOrElse(v,0L))
        def update(cmd: Command) = cmd match {
          case Array("set", x, v) => State(p + 1, args.updated(x, valOf(v)), sounds)
          case Array("add", x, v) => State(p + 1, args.updated(x, valOf(x) + valOf(v)), sounds)
          case Array("mul", x, v) => State(p + 1, args.updated(x, valOf(x) * valOf(v)), sounds)
          case Array("mod", x, v) => State(p + 1, args.updated(x, valOf(x) % valOf(v)), sounds)
          case Array("rcv", x)    => State(if(valOf(x) != 0) -1 else p + 1, args, sounds)
          case Array("snd", x)    => State(p + 1, args, valOf(x) :: sounds)
          case Array("jgz", x, v) => State( p + (if(valOf(x) > 0) valOf(v) else 1), args, sounds)
        }
      }

      case class Program(commands: List[Command]) {
        trait Wait
        case class State(p: Long, args: Map[String, Long], out: List[Long], in: List[Long]) {
          import scala.util.Try
          def valOf(v: String) = Try(v.toLong).toOption.getOrElse(args.getOrElse(v,0L))
          def update: State = commands(p.toInt) match {
            case Array("set", x, v) => State(p + 1, args.updated(x, valOf(v)), out, in)
            case Array("add", x, v) => State(p + 1, args.updated(x, valOf(x) + valOf(v)), out, in)
            case Array("mul", x, v) => State(p + 1, args.updated(x, valOf(x) * valOf(v)), out, in)
            case Array("mod", x, v) => State(p + 1, args.updated(x, valOf(x) % valOf(v)), out, in)
            case Array("rcv", x)    =>
              if(in.nonEmpty) State(p + 1, args.updated(x, in.head), out, in.tail )
              else new State(p, args, out, Nil ) with Wait
            case Array("snd", x)    => State(p + 1, args, valOf(x) :: out, in)
            case Array("jgz", x, v) => State( p + (if(valOf(x) > 0) valOf(v) else 1), args, out, in)
          }
          def run = Stream.iterate(this)(_.update)
            //.map(_.trace)
            .collect{ case s: State with Wait => s }.head
        }

        val start0: State = State(0, Map("p" -> 0), List.empty, List.empty).run
        val start1: State = State(0, Map("p" -> 1), List.empty, List.empty).run
        lazy val answer =
          Stream.iterate(start0 -> start1){ case (x,y) =>
            x.copy(in = y.out, out = Nil).run -> y.copy(in = x.out, out = Nil).run }
           // .map(_.trace)
          .takeWhile{ case (x,y) => x.out.nonEmpty || y.out.nonEmpty}
          .foldLeft(0){case (res,(x,y)) => res.trace + y.out.length}
      }

      //Program("snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d".split("\n").map(_.split(" ")).toList).answer.trace
      Program(Input.day(18).split("\n").map(_.split(" ")).toList).answer.trace

      //Stream.iterate(State2(0, Map.empty, Nil, Nil))(x => update(testCommands))
      def run(commands: List[Command]): State => State = x => x.update(commands(x.p.toInt))
      def answer1(commands: List[Command]) = Iterator.iterate(State())(run(commands))
        .dropWhile(x => x.p > -1 && x.p < commands.length).next().sounds.head
      //answer1(testCommands).trace
      assert(answer1(testCommands) == 4)
      assert(answer1(commands) == 8600)

      //      Iterator.iterate(State())(run(commands))
//        .dropWhile(x => x.p > -1 && x.p < commands.length).next().valOf("$").trace
    }
    'Day19 - {
      //ImportInput.save(19)
      val maze = Input.day(19).split("\n").toVector
//      val maze =
//      ( "     |          \n" +
//        "     |  +--+    \n" +
//        "     A  |  C    \n" +
//        " F---|----E|--+ \n" +
//        "     |  |  |  D \n" +
//        "     +B-+  +--+ \n" +
//        "                ")
//        .split("\n").toVector

      sealed trait Dir {
        def left: Dir
        def right: Dir
        def symbol: Char
      }
//      object East extends Dir { override def left: Dir = ???  override def right: Dir = ??? }
      final case object East extends Dir { def left = North;  def right = South; def symbol = '-' }
      final case object West extends Dir { def left = South;  def right = North; def symbol = '-'  }
      final case object North extends Dir { def left = West;  def right = East; def symbol = '|'  }
      final case object South extends Dir { def left = East;  def right = West; def symbol = '|' }

      case class Point(x: Int, y: Int, dir: Dir) {
        lazy val fwd: Point = dir match {
          case East  => Point(x, y+1, dir)
          case West  => Point(x, y-1, dir)
          case North => Point(x-1, y, dir)
          case South => Point(x+1, y, dir)
        }
        lazy val left = Point(x,y,dir.left).fwd
        lazy val right = Point(x,y,dir.right).fwd
        lazy val value = maze.lift(x).flatMap(_.lift(y)).getOrElse(' ')
        def next =
          if(value == '+')
            if(left.value.trace((x+1,y+1)) == ' ' || left.value == dir.symbol) right
            else left
          else fwd
      }
      val startpoint = Point(0, maze(0).indexOf('|'), South)
      Stream.iterate(startpoint)(_.next)
        .map(_.value)
        .filter(_.isLetter).takeWhile(_ != 'P').mkString("")
      Stream.iterate(startpoint)(_.next)
        .map(_.value).takeWhile(_ != 'P').length
    }
    'Day20 - {
      //      ImportInput.save(20)
      case class Vector3D(x: Int, y: Int, z: Int) {
        def + (that: Vector3D) = Vector3D(x+that.x, y+that.y,z+that.z)
      }
      case class Particle(p: Vector3D, v: Vector3D, a: Vector3D){
        def next = Particle(p + v + a, v + a, a)
      }
      val negnumber = P("-".? ~ CharIn('0' to '9').rep()).!.map(_.toInt)
      val vector = P("<"~negnumber.rep(exactly = 3, sep = ",") ~">").map{case Seq(x,y,z) => Vector3D(x,y,z)}
      val particle = P("p=" ~ vector ~ ", v=" ~ vector ~ ", a=" ~ vector).map{case (p,v,a) => Particle(p,v,a)}

      particle.rep(sep="\n").parse("p=<1,2,3>, v=<4,5,6>, a=<7,8,9>").get.value.trace
      particle.rep(sep="\n").parse("p=<-11104,1791,5208>, v=<-6,36,-84>, a=<19,-5,-4>").get.value.trace
      val particles = particle.rep(sep="\n").map(x => x).parse(Input.day(20)).get.value
      def answer1 = particles.groupBy{
        case Particle(_, _, Vector3D(ax,ay,az)) => math.abs(ax) + math.abs(ay) + math.abs(az)}
        .minBy(_._1)._2
      def uniques(ps: Seq[Particle]) = ps.groupBy(_.p).filter(_._2.length == 1).flatMap(_._2).toList
      Stream.iterate(uniques(particles))(ps => uniques(ps.map(_.next))).take(100).foreach(_.length.trace)
    }
    'Day21 - {
      //ImportInput.save(21)
      //Input.day(21)
      val start = ".#./..#/###"
      //def rotate3(s: String) =
      //1 2  1 3  4 2  4 3  2 1  3 1  2 4  2 1
      //3 4  2 4  3 1  2 1  4 3  4 2  1 3  4 3
      trait Tile {
        def rotate: Tile
        def flip: Tile
      }
      case class Tile2[T](a1: (T,T), a2: (T,T)) extends Tile {
        def rotate = Tile2((a1._2,a2._2), (a1._1,a2._1))
        def flip = Tile2(a2,a1)
      }
      case class Tile3[T](a1: (T,T,T), a2: (T,T,T), a3: (T,T,T)) extends Tile {
        def rotate = Tile3((a1._3,a2._3,a3._3), (a1._2,a2._2,a3._2),(a1._1,a2._1,a3._1))
        def flip = Tile3(a3,a2,a1)
      }

      val symm: Tile => List[Tile] =
        s => Stream.iterate(s)(_.rotate).take(4).flatMap(x => List(x,x.flip)).toList

      val a = ".#.\n..#\n###".split("\n").toList
      //if(a.length % 2 == 0) ??? else ???

      //a.map(_.grouped(2)).grouped(2)
      a.map(_.grouped(3).toList).grouped(3).toList

      //[
      //  [
      //    [[1,2,3],[4,5,6],[7,8,9]],
      //    [[1,2,3],[4,5,6],[7,8,9]],
      //    [[1,2,3],[4,5,6],[7,8,9]]
      //  ],
      //  [
      //    [[1,2,3],[4,5,6],[7,8,9]],
      //    [[1,2,3],[4,5,6],[7,8,9]],
      //    [[1,2,3],[4,5,6],[7,8,9]]
      //  ],
      //  [
      //    [[1,2,3],[4,5,6],[7,8,9]],
      //    [[1,2,3],[4,5,6],[7,8,9]],
      //    [[1,2,3],[4,5,6],[7,8,9]]]
      //  ]
      //]
      val (n,m) = (a.length / 3,  a(0).length / 3)
      //(0 until n).map(i => (0 until m).map( j => a(i)(j)))
      val aa = (1 to 9).map(i => (1 to 9).map(j => (i,j))).traceWith(_.map(_.mkString).mkString("\n"))
      val splited = aa.map(_.grouped(3).toList).transpose.grouped(3).toList
      splited.traceWith(_.map(_.map(_.map(_.mkString).mkString("\n"))))

      // +++ === +++
      // +++ === +++
      // +++ === +++

      // === +++ ===
      // === +++ ===
      // === +++ ===

      // +++ === +++
      // +++ === +++
      // +++ === +++

//      val symmetries = List[String=>String](
//        identity(_),
//        flipX, flipY, flipXY,
//        flipX andThen flipY,
//        flipX andThen flipXY,
//        flipY andThen flipXY,
//        flipX andThen flipXY andThen flipX)
//      symmetries.map(f => f("12/34")).distinct.length
    }
    'Day22 - {
      //ImportInput.save(22)
      sealed trait Dir {
        def left: Dir
        def right: Dir
      }
      //      object East extends Dir { override def left: Dir = ???  override def right: Dir = ??? }
      final case object East extends Dir { def left = North;  def right = South }
      final case object West extends Dir { def left = South;  def right = North  }
      final case object North extends Dir { def left = West;  def right = East  }
      final case object South extends Dir { def left = East;  def right = West }

      case class Point(x: Int, y: Int, dir: Dir) {
        lazy val fwd: Point = dir match {
          case East  => Point(x, y+1, dir)
          case West  => Point(x, y-1, dir)
          case North => Point(x-1, y, dir)
          case South => Point(x+1, y, dir)
        }
        lazy val left = Point(x,y,dir.left).fwd
        lazy val right = Point(x,y,dir.right).fwd
        lazy val bwd = Point(x,y,dir.left.left).fwd
      }

      def answer(input: String, steps: Int) = {
        var count = 0
        var inp = input.split("\n").toList
        val (cx, cy) = (inp.length/2, inp(0).length/2)
        val field = inp.map(_.zipWithIndex).zipWithIndex.flatMap {
          case (xs,i) => xs.collect{ case ('#', j) => i -> j}
        }
        Iterator.iterate(field -> Point(cx,cy, North)) {
          case (f, v@Point(x,y,_)) =>
//            v.trace
//            f.trace
            if(f.contains((x,y))) f.filterNot(_ == (x,y)) -> v.right
            else { count += 1; ((x,y) :: f) -> v.left}
        }.take(steps+1).foreach(_ => ()).trace(count)
        count
      }
      def answer2(input: String, steps: Int) = {
        var countW = 0
        var countI = 0
        var inp = input.split("\n").toList
        val (cx, cy) = (inp.length/2, inp(0).length/2)
        val field = inp.map(_.zipWithIndex).zipWithIndex.flatMap {
          case (xs,i) => xs.collect{ case ('#', j) => i -> j}
        }.map(_ -> '#').toMap
        Iterator.iterate(field -> Point(cx,cy, North)){
          case (f, v@Point(x,y,_)) =>
            val xy = (x,y)
            val state = f.getOrElse(xy, '.')
            state match {
              case '.' => (f.updated(xy,'W'), v.left) <| (_ => countW += 1)
              case 'W' => (f.updated(xy,'#'), v.fwd) <| (_ => countI += 1)
              case '#' => (f.updated(xy,'F'), v.right)
              case 'F' => (f - xy, v.bwd)
          }
        }.take(steps+1).foreach(_ => ())//.trace(count)
        countI -> countW
      }
      answer2("..#\n#..\n...", 100)
//      answer2("..#\n#..\n...", 10000000)
      answer2(Input.day(22), 10000000)
//      answer("..#\n#..\n...", 1)
//      answer("..#\n#..\n...", 2)
//      answer("..#\n#..\n...", 3)
//      answer("..#\n#..\n...", 70)
//      answer("..#\n#..\n...", 10000)
//      answer(Input.day(22), 10000)
    }
    'Day23 - {
      //ImportInput.save(23)
      type Command = Array[String]
      val commands = Input.day(23).split("\n").map(_.split(" ")).toList

      case class Program(commands: List[Command]) {
        trait Wait
        var mulCount = 0
        case class State(p: Long, args: Map[String, Long]) {
          import scala.util.Try
          def valOf(v: String) = Try(v.toLong).toOption.getOrElse(args.getOrElse(v,0L))
          def update: State = commands(p.toInt) match {
            case Array("set", x, v) => State(p + 1, args.updated(x, valOf(v)))
            case Array("sub", x, v) => State(p + 1, args.updated(x, valOf(x) - valOf(v)))
            case Array("mul", x, v) =>
              mulCount += 1
              State(p + 1, args.updated(x, valOf(x) * valOf(v)))
            case Array("jnz", x, v) =>
              State( p + (if(valOf(x) != 0) valOf(v) else 1), args)
          }
        }
        def run = Stream.iterate(State(0,Map.empty))(_.update)
          .takeWhile(x => x.p >= 0 && x.p < commands.length).last

        def run2 = Stream.iterate(State(0,Map("a" -> 1)))(_.update <| (x => if(x.p == 23) x.trace ))
            //.take(1000000)
          .takeWhile(x => x.p >= 0 && x.p < commands.length).last

      }
      //(Program(commands.traceWith(_.length)) <| (_.run)).mulCount
      //Program(commands).run2.args

      val primes = read(pwd/'input/'primes).split("\n")
        .flatMap(_.split(' ').filter(_.nonEmpty).map(_.toInt)).toList
      val somePrimes = primes.dropWhile(_ < 106700).takeWhile(_ < 106700 + 17000) // 123700

      Iterator.iterate(106700)(_ + 17).take(1001).filter(!somePrimes.contains(_)).size
    }
    'Day24 - {
//      ImportInput.save(24)
      val data = Input.day(24).split("\n").map(_.split("/") match {case Array(x,y) => (x.toInt, y.toInt)})
        .toSet[(Int,Int)].map{
          case (x,y) if x < y => (x,y)
          case (x,y)  => (y,x)
        }
      //data.sortBy(_._1).mkString("\n").trace
      def f( w: Int, v: Int, bs: Set[(Int,Int)] ): Int = {
        val sbs = startBlocks(v, bs).map( x => f(w+x._1 + x._2, x._2, remove(x,bs) ))
        if(sbs.nonEmpty) sbs.max else w
      }
      def g( res: List[(Int,Int)], v: Int, bs: Set[(Int,Int)] ): List[(Int,Int)] =
        startBlocks(v, bs).map( x => g( x :: res, x._2, remove(x,bs) ))
        .fold(res){
          case (r1, r2) if (r1 lengthCompare r2.length) > 0 => r1
          case (r1, r2) if (r1 lengthCompare r2.length) < 0 => r2
          case (r1, r2) => if( r1.map(_.reduce(_ + _)).sum > r2.map(_.reduce(_ + _)).sum) r1 else r2
        }


      def remove(el: (Int,Int), from: Set[(Int,Int)]) =
        if(from.contains(el)) (from - el) else (from - el.swap)
      def startBlocks(v: Int, bs: Set[(Int,Int)]) = bs.collect {
        case (x,y) if x == v => (x,y)
        case (x,y) if y == v => (y,x)
      }

      f(0, 0, data)
      g(Nil,0,data).map(_.reduce(_ + _)).sum

    }
  }
}