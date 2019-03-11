/*
 * Copyright 2015 The Baseio Project
 *  
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *  
 *      http://www.apache.org/licenses/LICENSE-2.0
 *  
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package hello;

import java.io.IOException;
import java.util.Arrays;

import com.firenio.baseio.Options;
import com.firenio.baseio.codec.http11.HttpAttrListener;
import com.firenio.baseio.codec.http11.HttpCodec;
import com.firenio.baseio.codec.http11.HttpConnection;
import com.firenio.baseio.codec.http11.HttpContentType;
import com.firenio.baseio.codec.http11.HttpDateUtil;
import com.firenio.baseio.codec.http11.HttpFrame;
import com.firenio.baseio.codec.http11.HttpStatus;
import com.firenio.baseio.common.Util;
import com.firenio.baseio.component.Channel;
import com.firenio.baseio.component.ChannelAcceptor;
import com.firenio.baseio.component.ChannelEventListenerAdapter;
import com.firenio.baseio.component.Frame;
import com.firenio.baseio.component.IoEventHandle;
import com.firenio.baseio.component.NioEventLoopGroup;
import com.firenio.baseio.component.SocketOptions;
import com.firenio.baseio.log.DebugUtil;
import com.firenio.baseio.log.LoggerFactory;
import com.jsoniter.output.JsonStream;
import com.jsoniter.output.JsonStreamPool;
import com.jsoniter.spi.JsonException;

public class TestHttpLoadServer {

    static final byte[] STATIC_PLAINTEXT = "Hello, World!".getBytes();

    static class Message {

        private final String message;

        public Message(String message) {
            this.message = message;
        }

        public String getMessage() {
            return message;
        }

    }

    public static void main(String[] args) throws Exception {
        boolean lite = Util.getBooleanProperty("lite");
        boolean read = Util.getBooleanProperty("read");
        boolean pool = Util.getBooleanProperty("pool");
        boolean epoll = Util.getBooleanProperty("epoll");
        boolean direct = Util.getBooleanProperty("direct");
        boolean inline = Util.getBooleanProperty("inline");
        boolean unsafeBuf = Util.getBooleanProperty("unsafeBuf");
        int core = Util.getIntProperty("core", 1);
        int frame = Util.getIntProperty("frame", 16);
        int level = Util.getIntProperty("level", 1);
        int readBuf = Util.getIntProperty("readBuf", 16);
        LoggerFactory.setEnableSLF4JLogger(false);
        LoggerFactory.setLogLevel(LoggerFactory.LEVEL_INFO);
        Options.setBufAutoExpansion(false);
        Options.setDebugErrorLevel(level);
        Options.setChannelReadFirst(read);
        Options.setEnableEpoll(epoll);
        Options.setEnableUnsafeBuf(unsafeBuf);
        DebugUtil.info("lite: {}", lite);
        DebugUtil.info("read: {}", read);
        DebugUtil.info("pool: {}", pool);
        DebugUtil.info("core: {}", core);
        DebugUtil.info("frame: {}", frame);
        DebugUtil.info("level: {}", level);
        DebugUtil.info("direct: {}", direct);
        DebugUtil.info("inline: {}", inline);
        DebugUtil.info("readBuf: {}", readBuf);

        IoEventHandle eventHandle = new IoEventHandle() {

            @Override
            public void accept(Channel ch, Frame frame) throws Exception {
                HttpFrame f = (HttpFrame) frame;
                String action = f.getRequestURL();
                if ("/plaintext".equals(action)) {
                    f.setContent(STATIC_PLAINTEXT);
                    f.setContentType(HttpContentType.text_plain);
                    f.setConnection(HttpConnection.NONE);
                } else if ("/json".equals(action)) {
                    f.setContent(serializeMsg(new Message("Hello, World!")));
                    f.setContentType(HttpContentType.application_json);
                    f.setConnection(HttpConnection.NONE);
                } else {
                    System.err.println("404");
                    f.setContent("404,page not found!".getBytes());
                    f.setContentType(HttpContentType.text_plain);
                    f.setStatus(HttpStatus.C404);
                }
                f.setDate(HttpDateUtil.getDateLine());
                ch.writeAndFlush(f);
                ch.release(f);
            }

        };
        
        int fcache = 1024 * 16;
        int pool_cap = 1024 * 128;
        int pool_unit = 256;
        if (inline) {
            pool_cap = 1024 * 8;
            pool_unit = 256 * 16;
        }
        HttpDateUtil.start();
        NioEventLoopGroup group = new NioEventLoopGroup();
        ChannelAcceptor context = new ChannelAcceptor(group, 8080);
        group.setMemoryPoolCapacity(pool_cap);
        group.setEnableMemoryPoolDirect(direct);
        group.setEnableMemoryPool(pool);
        group.setMemoryPoolUnit(pool_unit);
        group.setWriteBuffers(32);
        group.setChannelReadBuffer(1024 * readBuf);
        group.setEventLoopSize(Util.availableProcessors() * core);
        group.setConcurrentFrameStack(false);
        context.addProtocolCodec(new HttpCodec("baseio", fcache, lite, inline));
        context.addChannelEventListener(new HttpAttrListener());
        context.addChannelEventListener(new ChannelEventListenerAdapter() {

            @Override
            public void channelOpened(Channel ch) throws Exception {
                ch.setOption(SocketOptions.TCP_NODELAY, 1);
                ch.setOption(SocketOptions.TCP_QUICKACK, 1);
                ch.setOption(SocketOptions.SO_KEEPALIVE, 0);
            }
        });
        context.setIoEventHandle(eventHandle);
        context.bind();
    }

    private static byte[] serializeMsg(Message obj) {
        JsonStream stream = JsonStreamPool.borrowJsonStream();
        try {
            stream.reset(null);
            stream.writeVal(Message.class, obj);
            return Arrays.copyOfRange(stream.buffer().data(), 0, stream.buffer().tail());
        } catch (IOException e) {
            throw new JsonException(e);
        } finally {
            JsonStreamPool.returnJsonStream(stream);
        }
    }

}
