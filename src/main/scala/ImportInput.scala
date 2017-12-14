import akka.Done
import akka.actor.ActorSystem
import akka.http.javadsl.model
import akka.http.javadsl.unmarshalling.Unmarshaller
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.Cookie
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{FileIO, Sink, Source}
import ammonite.ops._

import scala.concurrent.{ExecutionContextExecutor, Future}

/* Created on 05.12.17 */
object ImportInput {
  implicit val system: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContextExecutor = system.dispatcher
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val um: Unmarshaller[model.HttpEntity, String] = Unmarshaller.entityToString
  import org.gzb.utils.Core._
  val session = read(pwd / 'session)
  def save(day: Int, year: Int = 2017): Future[Unit] = {
    val filename = pwd /'input/year.toString/day.toString
    val request = HttpRequest(uri = Uri(s"https://adventofcode.com/$year/day/$day/input"))
      .addHeader(Cookie("session", session))
    Http().singleRequest(request).collect{
      case HttpResponse(StatusCodes.OK, headers, entity, _) => entity
    } .flatMap(Unmarshal(_).to[String])
      .map( x => if(x.last == '\n') x.init else x)
      .map(x => write.over(filename, x))
  }

  def send[T](answer: T, day: Int, level: Int, year: Int = 2017): Future[String] = {
    val request = HttpRequest(uri = Uri(s"https://adventofcode.com/$year/day/$day/answer"))
      .withMethod(HttpMethods.POST)
      .addHeader(Cookie("session", session))
      .withEntity(FormData("level" -> "1", "answer" -> answer.toString).toEntity.trace).trace
    Http().singleRequest(request).collect{
      case HttpResponse(StatusCodes.OK, headers, entity, _) => entity
    } .flatMap(Unmarshal(_).to[String])
  }

  def save2015: Future[Done] = Source(1 to 25).mapAsync(1)(save(_, 2015)).runWith(Sink.ignore)
  def save2016: Future[Done] = Source(1 to 25).mapAsync(1)(save(_, 2016)).runWith(Sink.ignore)
  def save2017: Future[Done] = Source(5 to 6).mapAsync(1)(save(_)).runWith(Sink.ignore)

}
