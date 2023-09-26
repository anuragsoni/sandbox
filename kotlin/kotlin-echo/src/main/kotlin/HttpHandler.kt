import io.netty.buffer.Unpooled
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.SimpleChannelInboundHandler
import io.netty.handler.codec.http.DefaultFullHttpResponse
import io.netty.handler.codec.http.HttpHeaderNames
import io.netty.handler.codec.http.HttpHeaderValues
import io.netty.handler.codec.http.HttpObject
import io.netty.handler.codec.http.HttpRequest
import io.netty.handler.codec.http.HttpResponseStatus
import kotlinx.coroutines.*

class HttpHandler(val handler: suspend (Request) -> Response) :
    SimpleChannelInboundHandler<HttpObject>() {

    private lateinit var dispatcher: ExecutorCoroutineDispatcher

    override fun channelActive(ctx: ChannelHandlerContext) {
        dispatcher = ctx.executor().asCoroutineDispatcher()
    }

    override fun channelRead0(ctx: ChannelHandlerContext, msg: HttpObject) {
        if (msg is HttpRequest) {
            val headers =
                msg.headers().asSequence().map { entry -> entry.key to entry.value }.toList()
            val request = Request(method = msg.method().name(), uri = msg.uri(), headers = headers)
            CoroutineScope(dispatcher).launch {
                val response = handler(request)
                val nettyResponse =
                    DefaultFullHttpResponse(
                        msg.protocolVersion(),
                        HttpResponseStatus.OK,
                        Unpooled.wrappedBuffer(response.body.encodeToByteArray()))
                response.headers.forEach { (key, data) -> nettyResponse.headers().set(key, data) }
                if (!msg.protocolVersion().isKeepAliveDefault) {
                    nettyResponse
                        .headers()
                        .set(HttpHeaderNames.CONNECTION, HttpHeaderValues.KEEP_ALIVE)
                }
                ctx.writeAndFlush(nettyResponse)
            }
        }
    }

    override fun channelReadComplete(ctx: ChannelHandlerContext) {
        CoroutineScope(dispatcher).launch { ctx.flush() }
    }

    @Deprecated("Deprecated in Java")
    override fun exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
        cause.printStackTrace()
    }
}
