import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.ChannelInitializer
import io.netty.channel.ChannelOption
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http.HttpServerCodec
import kotlinx.coroutines.asCoroutineDispatcher

val payload = "Hello World"

suspend fun httpHandler(request: Request): Response {
    return Response(
        status = 200,
        headers = listOf("Content-Length" to payload.length.toString()),
        body = payload)
}

fun main() {
    val port = System.getenv().get("PORT")?.toInt() ?: 8080

    val parentGroup = NioEventLoopGroup(1)
    val workerGroup = NioEventLoopGroup(Math.max(1, Runtime.getRuntime().availableProcessors() - 1))

    try {
        val bootstrap = ServerBootstrap()
        bootstrap
            .group(parentGroup, workerGroup)
            .channel(NioServerSocketChannel::class.java)
            .option(ChannelOption.SO_BACKLOG, 11_000)
            .childHandler(
                object : ChannelInitializer<SocketChannel>() {
                    override fun initChannel(ch: SocketChannel) {
                        val pipeline = ch.pipeline()
                        pipeline.addLast(HttpServerCodec())
                        pipeline.addLast(
                            HttpHandler(workerGroup.asCoroutineDispatcher(), ::httpHandler))
                    }
                })

        val future = bootstrap.bind(port).sync()
        future.channel().closeFuture().sync()
    } finally {
        parentGroup.shutdownGracefully()
        workerGroup.shutdownGracefully()
    }
}
