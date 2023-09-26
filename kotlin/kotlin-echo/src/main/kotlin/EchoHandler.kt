import io.netty.channel.ChannelHandlerContext
import io.netty.channel.ChannelInboundHandlerAdapter
import kotlinx.coroutines.*

class EchoHandler(private val handler: suspend (String) -> String) :
    ChannelInboundHandlerAdapter() {

    private lateinit var dispatcher: ExecutorCoroutineDispatcher

    override fun channelActive(ctx: ChannelHandlerContext) {
        dispatcher = ctx.executor().asCoroutineDispatcher()
    }

    override fun channelRead(ctx: ChannelHandlerContext, msg: Any) {
        if (msg is String) {
            CoroutineScope(dispatcher).launch { ctx.write(handler(msg)) }
        }
    }

    override fun channelReadComplete(ctx: ChannelHandlerContext) {
        CoroutineScope(dispatcher).launch { ctx.flush() }
    }
}
