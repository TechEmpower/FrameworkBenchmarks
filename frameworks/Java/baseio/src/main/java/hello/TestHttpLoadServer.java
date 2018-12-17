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

import com.alibaba.fastjson.JSON;
import com.firenio.baseio.Options;
import com.firenio.baseio.buffer.ByteBuf;
import com.firenio.baseio.codec.http11.HttpCodec;
import com.firenio.baseio.codec.http11.HttpCodecLite;
import com.firenio.baseio.codec.http11.HttpFrameLite;
import com.firenio.baseio.codec.http11.HttpStatic;
import com.firenio.baseio.codec.http11.HttpStatus;
import com.firenio.baseio.common.Encoding;
import com.firenio.baseio.common.Util;
import com.firenio.baseio.component.ChannelAcceptor;
import com.firenio.baseio.component.IoEventHandle;
import com.firenio.baseio.component.NioEventLoopGroup;
import com.firenio.baseio.component.NioSocketChannel;
import com.firenio.baseio.log.DebugUtil;
import com.firenio.baseio.log.LoggerFactory;
import com.firenio.baseio.protocol.Frame;
import com.firenio.baseio.protocol.ProtocolCodec;

public class TestHttpLoadServer {

    static final byte[] STATIC_PLAINTEXT = "Hello, World!".getBytes(Encoding.UTF8);
    static final byte[] STATIC_SERVER    = "baseio".getBytes();

    public static void main(String[] args) throws Exception {
        boolean lite = Util.isSystemTrue("lite");
        boolean read = Util.isSystemTrue("read");
        boolean pool = Util.isSystemTrue("pool");
        boolean direct = Util.isSystemTrue("direct");
        int core = Util.getProperty("core", 1);
        int frame = Util.getProperty("frame", 0);
        int level = Util.getProperty("level");
        int readBuf = Util.getProperty("readBuf", 1);
        LoggerFactory.setEnableSLF4JLogger(false);
        LoggerFactory.setLogLevel(LoggerFactory.LEVEL_INFO);
        Options.setDebugErrorLevel(level);
        Options.setChannelReadFirst(read);
        DebugUtil.info("lite: {}", lite);
        DebugUtil.info("read: {}", read);
        DebugUtil.info("pool: {}", pool);
        DebugUtil.info("core: {}", core);
        DebugUtil.info("frame: {}", frame);
        DebugUtil.info("level: {}", level);
        DebugUtil.info("direct: {}", direct);
        DebugUtil.info("readBuf: {}", readBuf);

        IoEventHandle eventHandle = new IoEventHandle() {

            @Override
            public void accept(NioSocketChannel ch, Frame frame) throws Exception {
                HttpFrameLite f = (HttpFrameLite) frame;
                String action = f.getRequestURL();

                if ("/plaintext".equals(action)) {
                    f.write(STATIC_PLAINTEXT);
                    f.setContentType(HttpStatic.text_plain_bytes);
                } else if ("/json".equals(action)) {
                    f.write(JSON.toJSONString(new Message("Hello, World!")), ch);
                    f.setContentType(HttpStatic.application_json_bytes);
                } else {
                    f.write("404,page not found!", ch);
                    f.setStatus(HttpStatus.C404);
                }
                ByteBuf buf = ch.encode(f);
                ch.flush(buf);
                ch.release(f);
            }

        };

        ProtocolCodec codec = lite ? new HttpCodecLite("baseio", 1024 * frame)
                : new HttpCodec("baseio", 1024 * frame);
        NioEventLoopGroup group = new NioEventLoopGroup();
        group.setEnableMemoryPool(pool);
        group.setEnableMemoryPoolDirect(direct);
        group.setMemoryPoolCapacity(1024 * 128);
        group.setChannelReadBuffer(1024 * readBuf);
        group.setMemoryPoolUnit(256);
        group.setWriteBuffers(32);
        group.setEventLoopSize(Util.availableProcessors() * core);
        group.setConcurrentFrameStack(false);
        ChannelAcceptor context = new ChannelAcceptor(group, 8080);
        context.setProtocolCodec(codec);
        context.setIoEventHandle(eventHandle);
        context.bind();
    }

    static class Message {

        private final String message;

        public Message(String message) {
            this.message = message;
        }

        public String getMessage() {
            return message;
        }

    }

}
