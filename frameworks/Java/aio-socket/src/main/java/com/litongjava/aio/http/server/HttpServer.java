package com.litongjava.aio.http.server;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousChannelGroup;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.ZoneId;
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

    // 创建异步通道提供者
    EnhanceAsynchronousChannelProvider provider = new EnhanceAsynchronousChannelProvider(false);

    // 创建通道组
    AsynchronousChannelGroup group = provider.openAsynchronousChannelGroup(2, new ThreadFactory() {
      @Override
      public Thread newThread(Runnable r) {
        return new Thread(r, "http-server-thread");
      }
    });

    // 创建服务器通道并绑定端口
    EnhanceAsynchronousServerSocketChannel server = (EnhanceAsynchronousServerSocketChannel) provider.openAsynchronousServerSocketChannel(group);
    server.bind(new InetSocketAddress(8080), 0);

    System.out.println("HTTP Server 正在监听端口 8080 ...");

    // 异步接受连接
    server.accept(null, new CompletionHandler<AsynchronousSocketChannel, Object>() {
      @Override
      public void completed(AsynchronousSocketChannel channel, Object attachment) {
        // 继续接收其他连接
        server.accept(null, this);
        handleClient(channel);
      }

      @Override
      public void failed(Throwable exc, Object attachment) {
        exc.printStackTrace();
      }
    });

    // 主线程阻塞
    Thread.currentThread().join();
  }

  private static void handleClient(AsynchronousSocketChannel channel) {
    VirtualBuffer virtualBuffer = bufferPage.allocate(8192);
    ByteBuffer buffer = virtualBuffer.buffer();

    channel.read(buffer, virtualBuffer, new CompletionHandler<Integer, VirtualBuffer>() {
      @Override
      public void completed(Integer result, VirtualBuffer attachment) {
        try {
          if (result > 0) {
            buffer.flip();
            byte[] bytes = new byte[buffer.remaining()];
            buffer.get(bytes);
            String request = new String(bytes, StandardCharsets.UTF_8);
            
            // 生成当前时间，格式为 RFC 1123 格式
            String date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now(ZoneId.of("GMT")));
            
            ByteBuffer responseBuffer;
            if (request.startsWith("GET /plaintext")) {
              String body = "Hello, World!";
              String httpResponse = "HTTP/1.1 200 OK\r\n" +
                  "Content-Length: " + body.getBytes(StandardCharsets.UTF_8).length + "\r\n" +
                  "Server: aio-socket\r\n" +
                  "Content-Type: text/plain\r\n" +
                  "Date: " + date + "\r\n" +
                  "\r\n" +
                  body;
              responseBuffer = ByteBuffer.wrap(httpResponse.getBytes(StandardCharsets.UTF_8));

            } else if (request.startsWith("GET /json")) {
              String jsonString = JSON.toJSONString(new Message(HELLO_WORLD));
              int length = jsonString.getBytes(StandardCharsets.UTF_8).length;
              String httpResponse = "HTTP/1.1 200 OK\r\n" +
                  "Content-Length: " + length + "\r\n" +
                  "Server: aio-socket\r\n" +
                  "Content-Type: application/json\r\n" +
                  "Date: " + date + "\r\n" +
                  "\r\n" +
                  jsonString;
              responseBuffer = ByteBuffer.wrap(httpResponse.getBytes(StandardCharsets.UTF_8));
            } else {
              String body = "Hello, World!";
              String httpResponse = "HTTP/1.1 200 OK\r\n" +
                  "Content-Length: " + body.getBytes(StandardCharsets.UTF_8).length + "\r\n" +
                  "Content-Type: text/plain\r\n" +
                  "Date: " + date + "\r\n" +
                  "\r\n" +
                  body;
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
                  attachment.clean();
                }
              }
            });
          } else {
            try {
              channel.close();
            } catch (IOException e) {
              e.printStackTrace();
            }
          }
        } finally {
          // 注意：如果在写操作中已经归还了虚拟缓冲区，则不要重复释放
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
