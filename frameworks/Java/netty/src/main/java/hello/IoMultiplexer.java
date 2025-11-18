package hello;

import hello.loom.MultithreadVirtualEventExecutorGroup;
import io.netty.channel.IoHandlerFactory;
import io.netty.channel.MultiThreadIoEventLoopGroup;
import io.netty.channel.ServerChannel;
import io.netty.channel.epoll.Epoll;
import io.netty.channel.epoll.EpollIoHandler;
import io.netty.channel.epoll.EpollServerSocketChannel;
import io.netty.channel.kqueue.KQueue;
import io.netty.channel.kqueue.KQueueIoHandler;
import io.netty.channel.kqueue.KQueueServerSocketChannel;
import io.netty.channel.nio.NioIoHandler;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.channel.uring.IoUring;
import io.netty.channel.uring.IoUringIoHandler;
import io.netty.channel.uring.IoUringServerSocketChannel;

public enum IoMultiplexer {
   EPOLL, KQUEUE, JDK, IO_URING;

   public Class<? extends ServerChannel> serverChannelClass() {
      return switch (this) {
         case EPOLL -> EpollServerSocketChannel.class;
         case KQUEUE -> KQueueServerSocketChannel.class;
         case JDK -> NioServerSocketChannel.class;
         case IO_URING -> IoUringServerSocketChannel.class;
      };
   }

   public IoHandlerFactory newIoHandlerFactory() {
      return switch (this) {
         case EPOLL -> EpollIoHandler.newFactory();
         case KQUEUE -> KQueueIoHandler.newFactory();
         case JDK -> NioIoHandler.newFactory();
         case IO_URING -> IoUringIoHandler.newFactory();
      };
   }

   public MultiThreadIoEventLoopGroup newEventLoopGroup(int nThreads) {
      return new MultiThreadIoEventLoopGroup(nThreads, newIoHandlerFactory());
   }

   public MultithreadVirtualEventExecutorGroup newVirtualEventExecutorGroup(int nThreads) {
      return new MultithreadVirtualEventExecutorGroup(nThreads, newIoHandlerFactory());
   }

   public static IoMultiplexer type() {
      if (IoUring.isAvailable()) {
         return IO_URING;
      } else if (Epoll.isAvailable()) {
         return EPOLL;
      } else if (KQueue.isAvailable()) {
         return KQUEUE;
      } else {
         return JDK;
      }
   }
}