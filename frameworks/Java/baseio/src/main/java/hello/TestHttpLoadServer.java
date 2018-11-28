/*
 * Copyright 2015-2017 GenerallyCloud.com
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
import com.generallycloud.baseio.Options;
import com.generallycloud.baseio.buffer.ByteBuf;
import com.generallycloud.baseio.codec.http11.HttpCodec;
import com.generallycloud.baseio.codec.http11.HttpFrame;
import com.generallycloud.baseio.codec.http11.HttpHeader;
import com.generallycloud.baseio.codec.http11.HttpStatic;
import com.generallycloud.baseio.codec.http11.HttpStatus;
import com.generallycloud.baseio.common.Encoding;
import com.generallycloud.baseio.component.ChannelAcceptor;
import com.generallycloud.baseio.component.IoEventHandle;
import com.generallycloud.baseio.component.NioEventLoopGroup;
import com.generallycloud.baseio.component.NioSocketChannel;
import com.generallycloud.baseio.log.LoggerFactory;
import com.generallycloud.baseio.protocol.Frame;

public class TestHttpLoadServer {

    static final byte[] STATIC_PLAINTEXT = "Hello, World!".getBytes(Encoding.UTF8);
    static final byte[] STATIC_SERVER    = "baseio".getBytes();

    public static void main(String[] args) throws Exception {
        LoggerFactory.setEnableSLF4JLogger(false);
        LoggerFactory.setLogLevel(LoggerFactory.LEVEL_INFO);
        Options.setDevelopDebug(false);
        Options.setChannelReadFirst(true);

        IoEventHandle eventHandle = new IoEventHandle() {

            @Override
            public void accept(NioSocketChannel ch, Frame frame) throws Exception {
                HttpFrame f = (HttpFrame) frame;
                String action = f.getRequestURI();
                f.getResponseHeaders().remove(HttpHeader.Connection);
                f.setResponseHeader(HttpHeader.Server, STATIC_SERVER);

                if ("/plaintext".equals(action)) {
                    f.write(STATIC_PLAINTEXT);
                    f.setResponseHeader(HttpHeader.Content_Type, HttpStatic.text_plain_bytes);
                } else if ("/json".equals(action)) {
                    f.write(JSON.toJSONString(new Message("Hello, World!")), ch);
                    f.setResponseHeader(HttpHeader.Content_Type, HttpStatic.application_json_bytes);
                } else {
                    f.write("404,page not found!", ch);
                    f.setStatus(HttpStatus.C404);
                }
                ByteBuf buf = ch.encode(f);
                ch.flush(buf);
                ch.release(f);
            }

        };

        int core_size = Runtime.getRuntime().availableProcessors();
        NioEventLoopGroup group = new NioEventLoopGroup();
        group.setMemoryPoolCapacity(1024 * 1024 / core_size);
        group.setMemoryPoolUnit(512);
        group.setEventLoopSize(core_size);
        ChannelAcceptor context = new ChannelAcceptor(group, 8080);
        context.setProtocolCodec(new HttpCodec(1024 * 16));
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
