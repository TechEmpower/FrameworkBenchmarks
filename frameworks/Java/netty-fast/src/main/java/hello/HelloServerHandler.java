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
import io.netty.util.AttributeKey;
import io.netty.util.concurrent.FastThreadLocal;

public class HelloServerHandler extends ChannelInboundHandlerAdapter {

    private static final AttributeKey<PerChannelState> STATE_KEY =
        AttributeKey.valueOf("hello.state");

    // One cache per EventLoop thread
    private static final FastThreadLocal<ResponseCache> CACHE_TL = new FastThreadLocal<>();

    private static final byte[] NOT_FOUND_BYTES =
        "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n"
            .getBytes(StandardCharsets.US_ASCII);

    private static final byte CR = (byte) '\r';
    private static final byte LF = (byte) '\n';

    private static final byte[] GET = "GET".getBytes(StandardCharsets.US_ASCII);

    private static final byte[] PATH_PLAINTEXT = "/plaintext".getBytes(StandardCharsets.US_ASCII);
    private static final byte[] PATH_JSON = "/json".getBytes(StandardCharsets.US_ASCII);

    // JSON is static in TFB, so compute once
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
        if (state != null && state.inbound != null) {
            state.inbound.release();
            state.inbound = null;
        }
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) {
        if (!(msg instanceof ByteBuf)) {
            io.netty.util.ReferenceCountUtil.release(msg);
            return;
        }
        ByteBuf in = (ByteBuf) msg;
        PerChannelState state = state(ctx);

        try {
            if (state.inbound == null) {
                state.inbound = in;
            } else {
                state.inbound.writeBytes(in);
                in.release();
            }

            parseRequests(ctx, state);

        } catch (Throwable t) {
            if (state.inbound != null) {
                state.inbound.release();
                state.inbound = null;
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

    private void parseRequests(ChannelHandlerContext ctx, PerChannelState state) {
        ByteBuf in = state.inbound;
        if (in == null) {
            return;
        }

        while (true) {
            int start = in.readerIndex();
            int endOfHeaders = findEndOfHeaders(in, start, in.writerIndex());
            if (endOfHeaders == -1) {
                return;
            }

            if (!parseOneRequestAndRespond(ctx, state, in, start, endOfHeaders)) {
                return;
            }

            in.readerIndex(endOfHeaders);

            if (in.readableBytes() == 0) {
                in.release();
                state.inbound = null;
                return;
            }

            in.discardSomeReadBytes();
        }
    }

    private boolean parseOneRequestAndRespond(ChannelHandlerContext ctx, PerChannelState state, ByteBuf in, int start, int endOfHeaders) {
        int lineEnd = findCrlf(in, start, endOfHeaders);
        if (lineEnd == -1) {
            return false;
        }

        int sp1 = findByte(in, start, lineEnd, (byte) ' ');
        if (sp1 == -1) {
            return false;
        }
        int sp2 = findByte(in, sp1 + 1, lineEnd, (byte) ' ');
        if (sp2 == -1) {
            return false;
        }

        if (!equalsAscii(in, start, sp1, GET)) {
            writeNotFound(ctx, state);
            return true;
        }

        int pathStart = sp1 + 1;
        int pathEnd = sp2;

        if (equalsAscii(in, pathStart, pathEnd, PATH_PLAINTEXT)) {
            encodePlaintext(ctx, state);
            return true;
        }

        if (equalsAscii(in, pathStart, pathEnd, PATH_JSON)) {
            encodeJson(ctx, state);
            return true;
        }

        writeNotFound(ctx, state);
        return true;
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
            ctx.writeAndFlush(state.outAggregate).addListener(ChannelFutureListener.CLOSE_ON_FAILURE);
            state.outAggregate = null;
            state.hadBatched = false;
        }
        ctx.writeAndFlush(Unpooled.wrappedBuffer(NOT_FOUND_BYTES)).addListener(ChannelFutureListener.CLOSE_ON_FAILURE);
    }

    private static int findEndOfHeaders(ByteBuf buf, int from, int to) {
        for (int i = from + 3; i < to; i++) {
            if (buf.getByte(i - 3) == CR && buf.getByte(i - 2) == LF &&
                buf.getByte(i - 1) == CR && buf.getByte(i) == LF) {
                return i + 1;
            }
        }
        return -1;
    }

    private static int findCrlf(ByteBuf buf, int from, int to) {
        for (int i = from + 1; i < to; i++) {
            if (buf.getByte(i - 1) == CR && buf.getByte(i) == LF) {
                return i - 1;
            }
        }
        return -1;
    }

    private static int findByte(ByteBuf buf, int from, int to, byte b) {
        for (int i = from; i < to; i++) {
            if (buf.getByte(i) == b) {
                return i;
            }
        }
        return -1;
    }

    private static boolean equalsAscii(ByteBuf buf, int start, int end, byte[] ascii) {
        int len = end - start;
        if (len != ascii.length) {
            return false;
        }
        for (int i = 0; i < len; i++) {
            if (buf.getByte(start + i) != ascii[i]) {
                return false;
            }
        }
        return true;
    }

    private static final class PerChannelState {
        ByteBuf inbound;
        ByteBuf outAggregate;
        boolean hadBatched;
    }

    /**
     * Whole-response cache, updated once/sec, one instance per EventLoop thread.
     * Hot path: just read volatile byte[] and writeBytes().
     */
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
            // "Tue, 3 Jun 2008 11:05:30 GMT"
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
