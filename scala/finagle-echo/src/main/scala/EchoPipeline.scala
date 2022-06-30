import io.netty.channel.ChannelPipeline
import io.netty.handler.codec.string.{StringDecoder, StringEncoder}

case object EchoPipeline extends (ChannelPipeline => Unit) {
  override def apply(v1: ChannelPipeline): Unit = v1
    .addLast(new StringEncoder())
    .addLast(new StringDecoder())
}
