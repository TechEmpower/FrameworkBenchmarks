package hello;

import com.firenio.Options;
import com.firenio.buffer.ByteBuf;
import com.firenio.codec.http11.HttpAttachment;
import com.firenio.codec.http11.HttpCodec;
import com.firenio.codec.http11.HttpConnection;
import com.firenio.codec.http11.HttpContentType;
import com.firenio.codec.http11.HttpDateUtil;
import com.firenio.codec.http11.HttpFrame;
import com.firenio.codec.http11.HttpStatus;
import com.firenio.collection.ByteTree;
import com.firenio.common.Util;
import com.firenio.component.Channel;
import com.firenio.component.ChannelAcceptor;
import com.firenio.component.ChannelEventListenerAdapter;
import com.firenio.component.FastThreadLocal;
import com.firenio.component.Frame;
import com.firenio.component.IoEventHandle;
import com.firenio.component.NioEventLoopGroup;
import com.firenio.component.SocketOptions;
import com.firenio.log.DebugUtil;
import com.firenio.log.LoggerFactory;
import com.jsoniter.output.JsonStream;
import com.jsoniter.output.JsonStreamPool;
import com.jsoniter.spi.Slice;

public class TestHttpLoadServer {

    static final int    JSON_BUF         = FastThreadLocal.nextAttributeKey();
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

    static class MyHttpAttachment extends HttpAttachment {

        ByteBuf write_buf;

    }

    public static void main(String[] args) throws Exception {
        boolean lite      = Util.getBooleanProperty("lite");
        boolean read      = Util.getBooleanProperty("read");
        boolean pool      = Util.getBooleanProperty("pool");
        boolean epoll     = Util.getBooleanProperty("epoll");
        boolean nodelay   = Util.getBooleanProperty("nodelay");
        boolean cachedurl = Util.getBooleanProperty("cachedurl");
        boolean unsafeBuf = Util.getBooleanProperty("unsafeBuf");
        int     core      = Util.getIntProperty("core", 1);
        int     frame     = Util.getIntProperty("frame", 16);
        int     level     = Util.getIntProperty("level", 1);
        int     readBuf   = Util.getIntProperty("readBuf", 16);
        LoggerFactory.setEnableSLF4JLogger(false);
        LoggerFactory.setLogLevel(LoggerFactory.LEVEL_INFO);
        Options.setBufAutoExpansion(false);
        Options.setChannelReadFirst(read);
        Options.setEnableEpoll(epoll);
        Options.setEnableUnsafeBuf(unsafeBuf);
        Options.setBufFastIndexOf(true);
        DebugUtil.info("lite: {}", lite);
        DebugUtil.info("read: {}", read);
        DebugUtil.info("pool: {}", pool);
        DebugUtil.info("core: {}", core);
        DebugUtil.info("epoll: {}", epoll);
        DebugUtil.info("frame: {}", frame);
        DebugUtil.info("level: {}", level);
        DebugUtil.info("readBuf: {}", readBuf);
        DebugUtil.info("nodelay: {}", nodelay);
        DebugUtil.info("cachedurl: {}", cachedurl);
        DebugUtil.info("unsafeBuf: {}", unsafeBuf);

        int      processors = Util.availableProcessors() * core;
        int      fcache     = 1024 * 16;
        int      pool_unit  = 256 * 16;
        int      pool_cap   = 1024 * 8 * pool_unit * processors;
        String   server     = "tfb";
        ByteTree cachedUrls = null;
        if (cachedurl) {
            cachedUrls = new ByteTree();
            cachedUrls.add("/plaintext");
            cachedUrls.add("/json");
        }
        HttpCodec codec = new HttpCodec(server, fcache, lite, cachedUrls) {

            @Override
            protected Object newAttachment() {
                return new MyHttpAttachment();
            }
        };

        IoEventHandle eventHandle = new IoEventHandle() {

            @Override
            public void accept(Channel ch, Frame frame) throws Exception {
                HttpFrame f      = (HttpFrame) frame;
                String    action = f.getRequestURL();
                if ("/plaintext".equals(action)) {
                    MyHttpAttachment att = (MyHttpAttachment) ch.getAttachment();
                    ByteBuf          buf = att.write_buf;
                    if (buf == null) {
                        buf = ch.allocate();
                        ByteBuf temp = buf;
                        att.write_buf = buf;
                        ch.getEventLoop().submit(() -> {
                            ch.writeAndFlush(temp);
                            att.write_buf = null;
                        });
                    }
                    f.setContent(STATIC_PLAINTEXT);
                    f.setContentType(HttpContentType.text_plain);
                    f.setConnection(HttpConnection.NONE);
                    f.setDate(HttpDateUtil.getDateLine());
                    codec.encode(ch, buf, f);
                    codec.release(ch.getEventLoop(), f);
                } else if ("/json".equals(action)) {
                    ByteBuf temp = FastThreadLocal.get().getAttribute(JSON_BUF);
                    if (temp == null) {
                        temp = ByteBuf.heap(0);
                        FastThreadLocal.get().setAttribute(JSON_BUF, temp);
                    }
                    JsonStream stream = JsonStreamPool.borrowJsonStream();
                    try {
                        stream.reset(null);
                        stream.writeVal(Message.class, new Message("Hello, World!"));
                        Slice slice = stream.buffer();
                        temp.reset(slice.data(), slice.head(), slice.tail());
                        f.setContent(temp);
                        f.setContentType(HttpContentType.application_json);
                        f.setConnection(HttpConnection.NONE);
                        f.setDate(HttpDateUtil.getDateLine());
                        ch.writeAndFlush(f);
                        ch.release(f);
                    } finally {
                        JsonStreamPool.returnJsonStream(stream);
                    }
                } else {
                    System.err.println("404");
                    f.setString("404,page not found!", ch);
                    f.setContentType(HttpContentType.text_plain);
                    f.setStatus(HttpStatus.C404);
                    f.setDate(HttpDateUtil.getDateLine());
                    ch.writeAndFlush(f);
                    ch.release(f);
                }
            }

        };

        HttpDateUtil.start();
        NioEventLoopGroup group   = new NioEventLoopGroup();
        ChannelAcceptor   context = new ChannelAcceptor(group, 8080);
        group.setMemoryCapacity(pool_cap);
        group.setEnableMemoryPool(pool);
        group.setMemoryUnit(pool_unit);
        group.setWriteBuffers(32);
        group.setChannelReadBuffer(1024 * readBuf);
        group.setEventLoopSize(Util.availableProcessors() * core);
        group.setConcurrentFrameStack(false);
        if (nodelay) {
            context.addChannelEventListener(new ChannelEventListenerAdapter() {

                @Override
                public void channelOpened(Channel ch) throws Exception {
                    ch.setOption(SocketOptions.TCP_NODELAY, 1);
                    ch.setOption(SocketOptions.SO_KEEPALIVE, 0);
                }
            });
        }
        context.addProtocolCodec(codec);
        context.setIoEventHandle(eventHandle);
        context.bind(1024 * 8);
    }


}
