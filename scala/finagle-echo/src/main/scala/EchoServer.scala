import com.twitter.finagle.Service
import com.twitter.util.{Await, Future}

object EchoServer extends App {
  val service = new Service[String, String] {
    override def apply(request: String): Future[String] = Future.value(request)
  }

  val server = Echo.serve(":8080", service)
  Await.result(server)
}
