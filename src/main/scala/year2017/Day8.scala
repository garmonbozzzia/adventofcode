package year2017

/* Created on 12.12.17 */
object Day8 {
  import scala.collection.immutable.Map
  def process(state: Map[String, Int], command: String): Map[String,Int] = command.split(" ") match {
    case Array(nm1, op, vl1, "if", nm2, cond, vl2 ) =>
      def pred: String => (Int, Int) => Boolean = {
        case ("==") => _ == _
        case (">") => _ > _
        case ("<") => _ < _
        case (">=") => _ >= _
        case ("<=") => _ <= _
        case ("!=") => _ != _
      }
      def opF: String => (Int,Int) => Int = {
        case "dec" => _ - _
        case "inc" => _ + _
      }
      if(pred(cond)(state.getOrElse(nm2,0),vl2.toInt))
        state.updated(nm1, opF(op)(state.getOrElse(nm1,0),vl1.toInt)) else state
  }
}
