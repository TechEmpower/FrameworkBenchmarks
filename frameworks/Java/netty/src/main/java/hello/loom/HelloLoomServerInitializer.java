package hello.loom;

import java.util.concurrent.ScheduledExecutorService;

import hello.HelloServerHandler;
import hello.HelloServerInitializer;

public class HelloLoomServerInitializer extends HelloServerInitializer {

   private final MultithreadVirtualEventExecutorGroup group;

   public HelloLoomServerInitializer(MultithreadVirtualEventExecutorGroup group, ScheduledExecutorService service) {
      super(service);
      this.group = group;
   }

   @Override
   protected HelloServerHandler newHelloServerHandler(ScheduledExecutorService service) {
      return new VirtualThreadHelloServerHandler(service, group);
   }
}
