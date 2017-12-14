package year2015

object Day19 {
  type Transforms = Map[String, List[String]]
  type Transforms2 = List[(String, String)]
  def next(molecula: String, transforms: Transforms2): List[String] = transforms.flatMap {
    case (o,n) => (0 until molecula.length)
      .map(molecula.splitAt(_)).collect {
        case (xs, ys) if ys.startsWith(o) => xs + ys.replaceFirst(o,n)
      }
  }.distinct

  def parse(input: String): (String, List[(String, String)]) = {
    val data = input.split("\n").filter(_.nonEmpty)
    data.last -> data.init.map(_.split(" => ") match {
        case Array(x,y) => x -> y
      }).toList
  }

  type TransT[T] = T => T
  def reverse(xs: List[(String,String)]): List[(String, String)] = xs.map{case (x,y) => y -> x}
  def prev(start: String, end: String, rt: Transforms2 ): Int = {
      def f: TransT[(List[String], Set[String])] = {
        case (xs, cache) => xs.filterNot(cache.contains).filter(!_.contains("e")).flatMap(next(_,rt)).distinct -> (cache ++ xs)
      }
      Iterator.iterate(List(end) -> Set.empty[String])(f).take(10).takeWhile(!_._1.contains(start)).length
    }
  def solve2(input: String, start: String = "e"): Int = {
    val (end, t) = parse(input)
    prev(start, end, reverse(t))
  }
  def solve1(input: String): Int = {
    val (m, t) = parse(input)
    next(m,t).size
  }
}
