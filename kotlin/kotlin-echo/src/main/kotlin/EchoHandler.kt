import io.netty.channel.ChannelHandlerContext
import io.netty.channel.ChannelInboundHandlerAdapter
import kotlinx.coroutines.CoroutineDispatcher
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.launch

class EchoHandler(
    private val dispatcher: CoroutineDispatcher,
    private val handler: suspend (String) -> String
) : ChannelInboundHandlerAdapter() {
    override fun channelRead(ctx: ChannelHandlerContext, msg: Any) {
        if (msg is String) {
            CoroutineScope(dispatcher).launch { ctx.write(handler(msg)) }
        }
    }

    override fun channelReadComplete(ctx: ChannelHandlerContext) {
        CoroutineScope(dispatcher).launch { ctx.flush() }
    }
}
