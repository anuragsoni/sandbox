package example

import com.twitter.finagle.Service
import com.twitter.finagle.http
import com.twitter.util.Future
import com.twitter.finagle.http.path._
import com.twitter.finagle.Http
import com.twitter.util.Await
import com.twitter.conversions.DurationOps._
import com.twitter.server.TwitterServer

object Server extends TwitterServer {
  def greet(name: String) = new Service[http.Request, http.Response] {
    def apply(request: http.Request): Future[http.Response] = {
      val response = http.Response(http.Status.Ok)
      response.setContentString(s"Hello, $name")
      Future.value(response)
    }
  }

  val root = new Service[http.Request, http.Response] {
    def apply(request: http.Request): Future[http.Response] = {
      val response = http.Response(http.Status.Ok)
      response.setContentString("Hello World")
      Future.value(response)
    }
  }

  val service =
    http.service.RoutingService.byMethodAndPathObject {
      case (http.Method.Get, Root)                  => root
      case (http.Method.Get, Root / "greet" / name) => greet(name)
    }

  def main(): Unit = {
    val server =
      Http.server.withAdmissionControl
        .concurrencyLimit(maxConcurrentRequests = 1024)
        .withSession
        .maxLifeTime(20.seconds)
        .withSession
        .maxIdleTime(10.seconds)
        .withHttpStats
        .withHttp2
        .serve(":8080", service)

    Await.ready(server)
  }

}
