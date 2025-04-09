package hello.loom;

import java.util.ArrayDeque;
import java.util.concurrent.ScheduledExecutorService;

import com.jsoniter.output.JsonStream;

import hello.HelloServerHandler;
import hello.HttpResponses;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.util.AsciiString;

public class VirtualThreadHelloServerHandler extends HelloServerHandler {

   private final ArrayDeque<FullHttpResponse> responses = new ArrayDeque<>();
   private final MultithreadVirtualEventExecutorGroup group;

   public VirtualThreadHelloServerHandler(ScheduledExecutorService service, MultithreadVirtualEventExecutorGroup group) {
      super(service);
      this.group = group;
   }

   @Override
   protected void writePlainResponse(ChannelHandlerContext ctx, AsciiString date) {
      group.eventLoopVirtualThreadFactory().newThread(() -> {
         responses.add(HttpResponses.makePlaintextResponse(date));
      }).start();
   }

   @Override
   protected void writeJsonResponse(ChannelHandlerContext ctx, JsonStream stream, AsciiString date) {
      group.eventLoopVirtualThreadFactory().newThread(() -> {
         responses.add(HttpResponses.makeJsonResponse(stream, date));
      }).start();
   }

   @Override
   public void channelReadComplete(ChannelHandlerContext ctx) {
      var responses = this.responses;
      for (int i = 0, count = responses.size(); i < count; i++) {
         ctx.write(responses.poll());
      }
      ctx.flush();
   }
}
