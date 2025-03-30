package com.litongjava.aio.http.server;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousChannelGroup;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ThreadFactory;

import com.alibaba.fastjson.JSON;
import com.litongjava.aio.http.server.model.Message;
import com.litongjava.enhance.buffer.BufferPage;
import com.litongjava.enhance.buffer.BufferPagePool;
import com.litongjava.enhance.buffer.VirtualBuffer;
import com.litongjava.enhance.channel.EnhanceAsynchronousChannelProvider;
import com.litongjava.enhance.channel.EnhanceAsynchronousServerSocketChannel;

public class HttpServer {

  private static int cpuNum = Runtime.getRuntime().availableProcessors();
  private static BufferPagePool pool = new BufferPagePool(0, 1024 * cpuNum, true);
  private static BufferPage bufferPage = pool.allocateBufferPage();
  private static final String HELLO_WORLD = "Hello, World!";

  public static void main(String[] args) throws Exception {

    // 创建通道提供者，false 表示非低内存模式
    EnhanceAsynchronousChannelProvider provider = new EnhanceAsynchronousChannelProvider(false);

    // 创建一个异步通道组，线程数设为2（根据需求调整）
    AsynchronousChannelGroup group = provider.openAsynchronousChannelGroup(2, new ThreadFactory() {
      @Override
      public Thread newThread(Runnable r) {
        return new Thread(r, "http-server-thread");
      }
    });

    // 使用提供者创建服务器通道
    EnhanceAsynchronousServerSocketChannel server = (EnhanceAsynchronousServerSocketChannel) provider.openAsynchronousServerSocketChannel(group);
    // 绑定端口，例如 80，设置 backlog
    server.bind(new InetSocketAddress(8080), 0);

    System.out.println("HTTP Server 正在监听端口 8080 ...");

    // 异步接受连接请求
    server.accept(null, new CompletionHandler<AsynchronousSocketChannel, Object>() {
      @Override
      public void completed(AsynchronousSocketChannel channel, Object attachment) {
        // 接收到连接后，立即继续接受下一个连接
        server.accept(null, this);
        // 处理客户端连接
        handleClient(channel);
      }

      @Override
      public void failed(Throwable exc, Object attachment) {
        exc.printStackTrace();
      }
    });

    // 主线程阻塞，以保证服务器运行
    Thread.currentThread().join();
  }

  private static void handleClient(AsynchronousSocketChannel channel) {
    // 通过 BufferPage 池化获取一个 VirtualBuffer，分配 8192 字节空间
    VirtualBuffer virtualBuffer = bufferPage.allocate(8192);
    ByteBuffer buffer = virtualBuffer.buffer();

    // 异步读取客户端请求，将 VirtualBuffer 作为附件传入
    channel.read(buffer, virtualBuffer, new CompletionHandler<Integer, VirtualBuffer>() {
      @Override
      public void completed(Integer result, VirtualBuffer attachment) {
        try {
          if (result > 0) {
            buffer.flip();
            byte[] bytes = new byte[buffer.remaining()];
            buffer.get(bytes);
            // 将请求转换成字符串进行判断
            String request = new String(bytes, StandardCharsets.UTF_8);

            ByteBuffer responseBuffer;
            // 如果请求以 "GET /plaintext" 开头，则直接返回 HELLO_WORLD_BYTES
            if (request.startsWith("GET /plaintext")) {
              String httpResponse = "HTTP/1.1 200 OK\r\n" + "Content-Length: 13\r\n" + "Server: aio-socket\r\n" + "Content-Type: text/plain\r\n" + "\r\n" + "Hello, World!";
              responseBuffer = ByteBuffer.wrap(httpResponse.getBytes(StandardCharsets.UTF_8));

            } else if (request.startsWith("GET /json")) {
              String jsonString = JSON.toJSONString(new Message(HELLO_WORLD));
              int length = jsonString.length();
              String httpResponse = "HTTP/1.1 200 OK\r\n" + "Content-Length: " + length + "\r\n" + "Server: aio-socket\r\n" + "Content-Type: application/json\r\n" + "\r\n" + jsonString;

              responseBuffer = ByteBuffer.wrap(httpResponse.getBytes(StandardCharsets.UTF_8));

            } else {
              // 默认返回完整的 HTTP 响应
              String httpResponse = "HTTP/1.1 200 OK\r\n" + "Content-Length: 13\r\n" + "Content-Type: text/plain\r\n" + "\r\n" + "Hello, World!";
              responseBuffer = ByteBuffer.wrap(httpResponse.getBytes(StandardCharsets.UTF_8));
            }

            // 异步写响应
            channel.write(responseBuffer, attachment, new CompletionHandler<Integer, VirtualBuffer>() {
              @Override
              public void completed(Integer result, VirtualBuffer attachment) {
                try {
                  channel.close();
                } catch (IOException e) {
                  e.printStackTrace();
                } finally {
                  // 写完响应后归还虚拟缓冲区
                  attachment.clean();
                }
              }

              @Override
              public void failed(Throwable exc, VirtualBuffer attachment) {
                exc.printStackTrace();
                try {
                  channel.close();
                } catch (IOException e) {
                  e.printStackTrace();
                } finally {
                  // 出现写异常时也归还虚拟缓冲区
                  attachment.clean();
                }
              }
            });
          } else {
            // 未读到数据，则关闭连接
            try {
              channel.close();
            } catch (IOException e) {
              e.printStackTrace();
            }
          }
        } finally {
          // 注意：如果在写操作中已经归还了虚拟缓冲区，则不要重复释放
          // attachment.clean();
        }
      }

      @Override
      public void failed(Throwable exc, VirtualBuffer attachment) {
        exc.printStackTrace();
        try {
          channel.close();
        } catch (IOException e) {
          e.printStackTrace();
        } finally {
          attachment.clean();
        }
      }
    });
  }
}
