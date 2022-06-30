import com.twitter.finagle.{ListeningServer, Server, ServiceFactory}

import java.net.SocketAddress
import com.twitter.finagle.Stack
import com.twitter.finagle.server.StackServer
import com.twitter.finagle.server.StdStackServer
import com.twitter.finagle.server.Listener
import com.twitter.finagle.Service
import com.twitter.finagle.transport.{Transport, TransportContext}
import com.twitter.util.Closable
import com.twitter.finagle.netty4.Netty4Listener
import com.twitter.finagle.dispatch.SerialServerDispatcher

object Echo extends Server[String, String] {

  case class Server(
      stack: Stack[ServiceFactory[String, String]] = StackServer.newStack,
      params: Stack.Params = StackServer.defaultParams
  ) extends StdStackServer[String, String, Server] {
    protected type In = String
    protected type Out = String
    protected type Context = TransportContext

    override protected def copy1(
        stack: Stack[ServiceFactory[String, String]],
        params: Stack.Params = this.params
    ): Server =
      copy(stack, params)

    override def newListener(): Listener[String, String, TransportContext] =
      Netty4Listener(EchoPipeline, params)

    override protected def newDispatcher(
        transport: Transport[String, String] {
          type Context <: TransportContext
        },
        service: Service[String, String]
    ): Closable = new SerialServerDispatcher(transport, service)
  }

  val server = Server()

  override def serve(
      addr: SocketAddress,
      service: ServiceFactory[String, String]
  ): ListeningServer =
    server.serve(addr, service)
}
