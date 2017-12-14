package year2017

object Day4 extends Day {
  override type InputType = List[List[String]]
  override def parse: String => InputType = _.split("\n").toList.map(_.split(" ").toList)
  def isValid1(words: List[String]): Boolean = words.distinct.lengthCompare(words.size) == 0
  def isValid2(words: List[String]): Boolean = words.map(_.sorted).distinct.lengthCompare(words.size) == 0
  def solve1(input: InputType): Int = input.count(isValid1)
  def solve2(input: InputType): Int = input.count(isValid2)
}
