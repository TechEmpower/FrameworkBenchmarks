package hello;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.handler.codec.http.HttpContent;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.HttpUtil;
import io.netty.handler.codec.http.LastHttpContent;
import io.netty.util.AttributeKey;
import io.netty.util.concurrent.FastThreadLocal;

public class HelloServerHandler extends ChannelInboundHandlerAdapter {

    private static final AttributeKey<PerChannelState> STATE_KEY =
        AttributeKey.valueOf("hello.state");

    private static final FastThreadLocal<ResponseCache> CACHE_TL = new FastThreadLocal<>();

    private static final byte[] NOT_FOUND_BYTES =
        "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n"
            .getBytes(StandardCharsets.US_ASCII);

    private static final byte[] JSON_BODY = JsonUtils.serializeMsg(Constants.STATIC_MESSAGE);

    private final ResponseCache cache;

    public HelloServerHandler(ScheduledExecutorService scheduledExecutor) {
        ResponseCache c = CACHE_TL.get();
        if (c == null) {
            c = new ResponseCache(scheduledExecutor);
            CACHE_TL.set(c);
        }
        this.cache = c;
    }

    @Override
    public void channelActive(ChannelHandlerContext ctx) {
        ctx.channel().attr(STATE_KEY).set(new PerChannelState());
    }

    @Override
    public void channelInactive(ChannelHandlerContext ctx) {
        PerChannelState state = ctx.channel().attr(STATE_KEY).get();
        if (state != null && state.outAggregate != null) {
            state.outAggregate.release();
            state.outAggregate = null;
            state.hadBatched = false;
        }
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) {
        try {
            if (msg instanceof HttpRequest req) {
                handleRequest(ctx, state(ctx), req);
                return;
            }

            if (msg instanceof HttpContent content) {
                content.release();
                if (msg instanceof LastHttpContent) {
                    return;
                }
                return;
            }

            io.netty.util.ReferenceCountUtil.release(msg);
        } catch (Throwable t) {
            PerChannelState st = ctx.channel().attr(STATE_KEY).get();
            if (st != null && st.outAggregate != null) {
                st.outAggregate.release();
                st.outAggregate = null;
                st.hadBatched = false;
            }
            ctx.close();
        }
    }

    @Override
    public void channelReadComplete(ChannelHandlerContext ctx) {
        PerChannelState state = state(ctx);
        if (state.hadBatched && state.outAggregate != null) {
            ByteBuf out = state.outAggregate;
            state.outAggregate = null;
            state.hadBatched = false;
            ctx.writeAndFlush(out).addListener(ChannelFutureListener.CLOSE_ON_FAILURE);
        } else {
            ctx.flush();
        }
    }

    private void handleRequest(ChannelHandlerContext ctx, PerChannelState state, HttpRequest req) {
        state.closeAfterFlush |= !HttpUtil.isKeepAlive(req);

        final String uri = req.uri();
        if ("/plaintext".equals(uri)) {
            encodePlaintext(ctx, state);
            return;
        }
        if ("/json".equals(uri)) {
            encodeJson(ctx, state);
            return;
        }

        writeNotFound(ctx, state);
    }

    private PerChannelState state(ChannelHandlerContext ctx) {
        PerChannelState st = ctx.channel().attr(STATE_KEY).get();
        if (st == null) {
            st = new PerChannelState();
            ctx.channel().attr(STATE_KEY).set(st);
        }
        return st;
    }

    private ByteBuf ensureAggregate(ChannelHandlerContext ctx, PerChannelState state) {
        if (state.outAggregate == null) {
            state.outAggregate = ctx.alloc().buffer(256);
        }
        return state.outAggregate;
    }

    private void encodePlaintext(ChannelHandlerContext ctx, PerChannelState state) {
        ByteBuf agg = ensureAggregate(ctx, state);
        agg.writeBytes(cache.plaintextResponseBytes());
        state.hadBatched = true;
    }

    private void encodeJson(ChannelHandlerContext ctx, PerChannelState state) {
        ByteBuf agg = ensureAggregate(ctx, state);
        agg.writeBytes(cache.jsonResponseBytes());
        state.hadBatched = true;
    }

    private void writeNotFound(ChannelHandlerContext ctx, PerChannelState state) {
        if (state.outAggregate != null) {
            ByteBuf out = state.outAggregate;
            state.outAggregate = null;
            state.hadBatched = false;

            if (state.closeAfterFlush) {
                ctx.write(out);
                ctx.write(Unpooled.wrappedBuffer(NOT_FOUND_BYTES));
                ctx.flush();
                ctx.close();
                state.closeAfterFlush = false;
                return;
            }

            ctx.writeAndFlush(out).addListener(ChannelFutureListener.CLOSE_ON_FAILURE);
        }

        if (state.closeAfterFlush) {
            ctx.writeAndFlush(Unpooled.wrappedBuffer(NOT_FOUND_BYTES))
               .addListener(ChannelFutureListener.CLOSE_ON_FAILURE)
               .addListener(ChannelFutureListener.CLOSE);
            state.closeAfterFlush = false;
        } else {
            ctx.writeAndFlush(Unpooled.wrappedBuffer(NOT_FOUND_BYTES))
               .addListener(ChannelFutureListener.CLOSE_ON_FAILURE);
        }
    }

    private static final class PerChannelState {
        ByteBuf outAggregate;
        boolean hadBatched;
        boolean closeAfterFlush;
    }


    private static final class ResponseCache implements Runnable {
        private static final DateTimeFormatter RFC_1123 =
            DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneOffset.UTC);

        private static final byte[] PLAIN_PREFIX =
            ("HTTP/1.1 200 OK\r\n" +
             "Server: Netty\r\n" +
             "Date: ").getBytes(StandardCharsets.US_ASCII);

        private static final byte[] PLAIN_MID =
            ("\r\n" +
             "Content-Type: text/plain\r\n" +
             "Content-Length: " + Constants.STATIC_PLAINTEXT_LEN + "\r\n" +
             "\r\n").getBytes(StandardCharsets.US_ASCII);

        private static final byte[] JSON_PREFIX =
            ("HTTP/1.1 200 OK\r\n" +
             "Server: Netty\r\n" +
             "Date: ").getBytes(StandardCharsets.US_ASCII);

        private static final byte[] JSON_MID =
            ("\r\n" +
             "Content-Type: application/json\r\n" +
             "Content-Length: " + JSON_BODY.length + "\r\n" +
             "\r\n").getBytes(StandardCharsets.US_ASCII);

        private final ScheduledExecutorService scheduler;

        private volatile byte[] plaintextResponse;
        private volatile byte[] jsonResponse;

        ResponseCache(ScheduledExecutorService scheduler) {
            this.scheduler = scheduler;
            rebuildNow(); // first responses already have Date
            this.scheduler.scheduleAtFixedRate(this, 1, 1, TimeUnit.SECONDS);
        }

        byte[] plaintextResponseBytes() {
            return plaintextResponse;
        }

        byte[] jsonResponseBytes() {
            return jsonResponse;
        }

        @Override
        public void run() {
            rebuildNow();
        }

        private void rebuildNow() {
            String date = RFC_1123.format(Instant.now());
            byte[] dateBytes = date.getBytes(StandardCharsets.US_ASCII);

            // plaintext: prefix + date + mid + body
            byte[] plain = new byte[PLAIN_PREFIX.length + dateBytes.length + PLAIN_MID.length + Constants.STATIC_PLAINTEXT_LEN];
            int p = 0;
            System.arraycopy(PLAIN_PREFIX, 0, plain, p, PLAIN_PREFIX.length); p += PLAIN_PREFIX.length;
            System.arraycopy(dateBytes, 0, plain, p, dateBytes.length); p += dateBytes.length;
            System.arraycopy(PLAIN_MID, 0, plain, p, PLAIN_MID.length); p += PLAIN_MID.length;
            System.arraycopy(Constants.STATIC_PLAINTEXT, 0, plain, p, Constants.STATIC_PLAINTEXT_LEN);

            // json: prefix + date + mid + body
            byte[] json = new byte[JSON_PREFIX.length + dateBytes.length + JSON_MID.length + JSON_BODY.length];
            int j = 0;
            System.arraycopy(JSON_PREFIX, 0, json, j, JSON_PREFIX.length); j += JSON_PREFIX.length;
            System.arraycopy(dateBytes, 0, json, j, dateBytes.length); j += dateBytes.length;
            System.arraycopy(JSON_MID, 0, json, j, JSON_MID.length); j += JSON_MID.length;
            System.arraycopy(JSON_BODY, 0, json, j, JSON_BODY.length);

            plaintextResponse = plain;
            jsonResponse = json;
        }
    }
}
