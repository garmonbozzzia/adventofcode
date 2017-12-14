package year2017

object Day3 {
  def solve1(point: Int): Int = toAxis(point) + ringNumber(point)
  def ringNumber(point: Int): Int = Iterator.iterate(0)(_ + 1).find(k => point <= (2*k+1)*(2*k+1)).get
  def toAxis(point: Int): Int = if(point==1) 0 else {
    val rn = ringNumber(point)
    val num = point - (2 * rn - 1) * (2 * rn - 1) - 1
    val k = 2 * rn
    val aNum = num%k
    val res = math.abs(aNum-(rn-1))
    //println(s"point=$point, rn=$rn, num=$num, aNum=$aNum, k=$k, res=${res}")
    res
  }

  type Point = (Int,Int)
  def next: Point => Point = {
    case (x,y) if y <= x && x + y <= 0 => (x + 1, y)
    case (x,y) if x > y && x + y > 0 => (x, y + 1)
    case (x,y) if y >= x && x + y > 0 => (x - 1, y)
    case (x,y) if x < y && x + y <=0 => (x, y - 1)
  }

  def neighbours: Point => List[Point] = {
    case (x,y) => List((x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1))
  }
  def solve1_2(point: Int): Int = Iterator.iterate((0,0))(next).drop(point-1).next() match {
    case (x,y) => math.abs(x) + math.abs(y)}
  def solve2(point: Int): Int = Iterator.iterate((1,0))(next).scanLeft(Map(((0,0),1)))((cache, p) =>
    cache.updated(p,neighbours(p).map(cache.getOrElse(_,0)).sum)
  ).dropWhile(_.values.max <= point).next().values.max
}
