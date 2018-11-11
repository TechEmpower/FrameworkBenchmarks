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
import com.generallycloud.baseio.Constants;
import com.generallycloud.baseio.buffer.ByteBuf;
import com.generallycloud.baseio.codec.http11.HttpHeader;
import com.generallycloud.baseio.codec.http11.HttpStatic;
import com.generallycloud.baseio.codec.http11.HttpStatus;
import com.generallycloud.baseio.codec.http11.ServerHttpCodec;
import com.generallycloud.baseio.codec.http11.ServerHttpFrame;
import com.generallycloud.baseio.common.Encoding;
import com.generallycloud.baseio.component.ChannelAcceptor;
import com.generallycloud.baseio.component.ChannelContext;
import com.generallycloud.baseio.component.IoEventHandle;
import com.generallycloud.baseio.component.NioEventLoopGroup;
import com.generallycloud.baseio.component.NioSocketChannel;
import com.generallycloud.baseio.log.LoggerFactory;
import com.generallycloud.baseio.protocol.Frame;

public class TestHttpLoadServer {
    
    static final byte[] STATIC_PLAINTEXT = "Hello, World!".getBytes(Encoding.UTF8);
    static final byte[] CONTENT_TYPE_JSON = "application/json".getBytes(Encoding.UTF8);
	
    public static void main(String[] args) throws Exception {
    	LoggerFactory.setLogLevel(LoggerFactory.LEVEL_ERROR);
    	System.setProperty(Constants.DEVELOP_DEBUG_KEY, "false");
        
        IoEventHandle eventHandle = new IoEventHandle() {

            @Override
            public void accept(NioSocketChannel ch, Frame frame) throws Exception {
            	ServerHttpFrame f = (ServerHttpFrame) frame;
            	String action = f.getRequestURI();
            	f.setResponseHeader(HttpHeader.Connection_Bytes, null);
                
            	if("/plaintext".equals(action)){
            		frame.write(STATIC_PLAINTEXT);
            		f.setResponseHeader(HttpHeader.Content_Type_Bytes, HttpStatic.plain_bytes);
            	}else if("/json".equals(action)){
            		frame.write(JSON.toJSONString(new Message("Hello, World!")), ch);
            		f.setResponseHeader(HttpHeader.Content_Type_Bytes, CONTENT_TYPE_JSON);
            	}else{
            		frame.write("404,page not found!",ch);
            		f.setStatus(HttpStatus.C404);
            	}
                ByteBuf buf = ch.encode(frame);
                ch.flush(buf);
                f.release(ch.getEventLoop());
            }

        };

        NioEventLoopGroup group = new NioEventLoopGroup();
        group.setMemoryPoolCapacity(1024 * 256);
        group.setMemoryPoolUnit(512);
        ChannelContext context = new ChannelContext(8080);
        ChannelAcceptor acceptor = new ChannelAcceptor(context, group);
        context.setProtocolCodec(new ServerHttpCodec(1024 * 8));
        context.setIoEventHandle(eventHandle);

        acceptor.bind();
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
