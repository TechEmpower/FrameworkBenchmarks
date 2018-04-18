/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import java.io.*;
import java.lang.reflect.Type;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.channels.*;
import java.nio.file.*;
import java.time.ZoneId;
import static java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.*;
import java.util.logging.Level;
import org.redkale.convert.*;
import org.redkale.convert.json.JsonConvert;
import org.redkale.net.*;
import org.redkale.util.AnyValue.DefaultAnyValue;
import org.redkale.util.AnyValue.Entry;
import org.redkale.util.*;

/**
 * Http响应包 与javax.servlet.http.HttpServletResponse 基本类似。  <br>
 * 同时提供发送json的系列接口: public void finishJson(Type type, Object obj)  <br>
 * Redkale提倡http+json的接口风格， 所以主要输出的数据格式为json， 同时提供异步接口。  <br>
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class HttpResponse extends Response<HttpContext, HttpRequest> {

    private static final ByteBuffer buffer304 = ByteBuffer.wrap("HTTP/1.1 304 Not Modified\r\nContent-Length:0\r\n\r\n".getBytes()).asReadOnlyBuffer();

    private static final ByteBuffer buffer404 = ByteBuffer.wrap("HTTP/1.1 404 Not Found\r\nContent-Length:0\r\n\r\n".getBytes()).asReadOnlyBuffer();

    protected static final byte[] status200Bytes = "HTTP/1.1 200 OK\r\n".getBytes();

    protected static final byte[] LINE = new byte[]{'\r', '\n'};

    protected static final byte[] serverNameBytes = ("Server: " + System.getProperty("http.response.header.server", "redkale" + "/" + Redkale.getDotedVersion()) + "\r\n").getBytes();

    protected static final byte[] connectCloseBytes = "Connection: close\r\n".getBytes();

    private static final Set<OpenOption> options = new HashSet<>();

    private static final Map<Integer, String> httpCodes = new HashMap<>();

    static {
        options.add(StandardOpenOption.READ);

        httpCodes.put(100, "Continue");
        httpCodes.put(101, "Switching Protocols");

        httpCodes.put(200, "OK");
        httpCodes.put(201, "Created");
        httpCodes.put(202, "Accepted");
        httpCodes.put(203, "Non-Authoritative Information");
        httpCodes.put(204, "No Content");
        httpCodes.put(205, "Reset Content");
        httpCodes.put(206, "Partial Content");

        httpCodes.put(300, "Multiple Choices");
        httpCodes.put(301, "Moved Permanently");
        httpCodes.put(302, "Found");
        httpCodes.put(303, "See Other");
        httpCodes.put(304, "Not Modified");
        httpCodes.put(305, "Use Proxy");
        httpCodes.put(307, "Temporary Redirect");

        httpCodes.put(400, "Bad Request");
        httpCodes.put(401, "Unauthorized");
        httpCodes.put(402, "Payment Required");
        httpCodes.put(403, "Forbidden");
        httpCodes.put(404, "Not Found");
        httpCodes.put(405, "Method Not Allowed");
        httpCodes.put(406, "Not Acceptable");
        httpCodes.put(407, "Proxy Authentication Required");
        httpCodes.put(408, "Request Timeout");
        httpCodes.put(409, "Conflict");
        httpCodes.put(410, "Gone");
        httpCodes.put(411, "Length Required");
        httpCodes.put(412, "Precondition Failed");
        httpCodes.put(413, "Request Entity Too Large");
        httpCodes.put(414, "Request URI Too Long");
        httpCodes.put(415, "Unsupported Media Type");
        httpCodes.put(416, "Requested Range Not Satisfiable");
        httpCodes.put(417, "Expectation Failed");

        httpCodes.put(500, "Internal Server Error");
        httpCodes.put(501, "Not Implemented");
        httpCodes.put(502, "Bad Gateway");
        httpCodes.put(503, "Service Unavailable");
        httpCodes.put(504, "Gateway Timeout");
        httpCodes.put(505, "HTTP Version Not Supported");
    }

    private static final ZoneId ZONE_GMT = ZoneId.of("GMT");

    private int status = 200;

    private String contentType = "";

    private long contentLength = -1;

    private HttpCookie[] cookies;

    private boolean headsended = false;

    private BiFunction<HttpResponse, ByteBuffer[], ByteBuffer[]> bufferHandler;
    //------------------------------------------------

    private final String plainContentType;

    private final byte[] plainContentTypeBytes;

    private final String jsonContentType;

    private final byte[] jsonContentTypeBytes;

    private final DefaultAnyValue header = new DefaultAnyValue();

    private final String[][] defaultAddHeaders;

    private final String[][] defaultSetHeaders;

    private final boolean autoOptions;

    private final HttpCookie defcookie;

    private final List<HttpRender> renders;

    private final boolean hasRender;

    private final HttpRender onlyoneHttpRender;

    public static ObjectPool<Response> createPool(AtomicLong creatCounter, AtomicLong cycleCounter, int max, Creator<Response> creator) {
        return new ObjectPool<>(creatCounter, cycleCounter, max, creator, (x) -> ((HttpResponse) x).prepare(), (x) -> ((HttpResponse) x).recycle());
    }

    public HttpResponse(HttpContext context, HttpRequest request,
        String plainContentType, String jsonContentType,
        String[][] defaultAddHeaders, String[][] defaultSetHeaders,
        HttpCookie defcookie, boolean autoOptions, List< HttpRender> renders) {
        super(context, request);
        this.plainContentType = plainContentType == null || plainContentType.isEmpty() ? "text/plain; charset=utf-8" : plainContentType;
        this.jsonContentType = jsonContentType == null || jsonContentType.isEmpty() ? "application/json; charset=utf-8" : jsonContentType;
        this.plainContentTypeBytes = ("Content-Type: " + this.plainContentType + "\r\n").getBytes();
        this.jsonContentTypeBytes = ("Content-Type: " + this.jsonContentType + "\r\n").getBytes();
        this.defaultAddHeaders = defaultAddHeaders;
        this.defaultSetHeaders = defaultSetHeaders;
        this.defcookie = defcookie;
        this.autoOptions = autoOptions;
        this.renders = renders;
        this.hasRender = renders != null && !renders.isEmpty();
        this.onlyoneHttpRender = renders != null && renders.size() == 1 ? renders.get(0) : null;
        this.contentType = this.plainContentType;
    }

    @Override
    protected AsyncConnection removeChannel() {
        return super.removeChannel();
    }

    protected AsyncConnection getChannel() {
        return channel;
    }

    @Override
    protected boolean recycle() {
        boolean rs = super.recycle();
        this.status = 200;
        this.contentLength = -1;
        this.contentType = null;
        this.cookies = null;
        this.headsended = false;
        this.header.clear();
        this.bufferHandler = null;
        return rs;
    }

    @Override
    protected void init(AsyncConnection channel) {
        super.init(channel);
    }

    /**
     * 获取状态码对应的状态描述
     *
     * @param status 状态码
     *
     * @return 状态描述
     */
    protected String getHttpCode(int status) {
        return httpCodes.get(status);
    }

    protected HttpRequest getRequest() {
        return request;
    }

    protected String getHttpCode(int status, String defValue) {
        String v = httpCodes.get(status);
        return v == null ? defValue : v;
    }

    @Override
    @SuppressWarnings("unchecked")
    protected void thenEvent(Servlet servlet) {
        this.servlet = servlet;
    }

    protected boolean isAutoOptions() {
        return this.autoOptions;
    }

    /**
     * 增加Cookie值
     *
     * @param cookies cookie
     *
     * @return HttpResponse
     */
    public HttpResponse addCookie(HttpCookie... cookies) {
        this.cookies = Utility.append(this.cookies, cookies);
        return this;
    }

    /**
     * 增加Cookie值
     *
     * @param cookies cookie
     *
     * @return HttpResponse
     */
    public HttpResponse addCookie(Collection<HttpCookie> cookies) {
        this.cookies = Utility.append(this.cookies, cookies);
        return this;
    }

    /**
     * 创建CompletionHandler实例
     *
     * @return CompletionHandler
     */
    public CompletionHandler createAsyncHandler() {
        return Utility.createAsyncHandler((v, a) -> {
            finish(v);
        }, (t, a) -> {
            context.getLogger().log(Level.WARNING, "Servlet occur, forece to close channel. request = " + request + ", result is CompletionHandler", (Throwable) t);
            finish(500, null);
        });
    }

    /**
     * 创建CompletionHandler子类的实例 <br>
     *
     * 传入的CompletionHandler子类必须是public，且保证其子类可被继承且completed、failed可被重载且包含空参数的构造函数。
     *
     * @param <H>          泛型
     * @param handlerClass CompletionHandler子类
     *
     * @return CompletionHandler
     */
    @SuppressWarnings("unchecked")
    public <H extends CompletionHandler> H createAsyncHandler(Class<H> handlerClass) {
        if (handlerClass == null || handlerClass == CompletionHandler.class) return (H) createAsyncHandler();
        return context.loadAsyncHandlerCreator(handlerClass).create(createAsyncHandler());
    }

    /**
     * 获取ByteBuffer生成器
     *
     * @return ByteBuffer生成器
     */
    public Supplier<ByteBuffer> getBufferSupplier() {
        return getBodyBufferSupplier();
    }

    /**
     * 将对象以JSON格式输出
     *
     * @param obj 输出对象
     */
    public void finishJson(final Object obj) {
        this.contentType = this.jsonContentType;
        if (this.recycleListener != null) this.output = obj;
        finish(request.getJsonConvert().convertTo(getBodyBufferSupplier(), obj));
    }

    /**
     * 将对象数组用Map的形式以JSON格式输出 <br>
     * 例如: finishMap("a",2,"b",3) 输出结果为 {"a":2,"b":3}
     *
     * @param objs 输出对象
     */
    public void finishMapJson(final Object... objs) {
        this.contentType = this.jsonContentType;
        if (this.recycleListener != null) this.output = objs;
        finish(request.getJsonConvert().convertMapTo(getBodyBufferSupplier(), objs));
    }

    /**
     * 将对象以JSON格式输出
     *
     * @param convert 指定的JsonConvert
     * @param obj     输出对象
     */
    public void finishJson(final JsonConvert convert, final Object obj) {
        this.contentType = this.jsonContentType;
        if (this.recycleListener != null) this.output = obj;
        finish(convert.convertTo(getBodyBufferSupplier(), obj));
    }

    /**
     * 将对象数组用Map的形式以JSON格式输出 <br>
     * 例如: finishMap("a",2,"b",3) 输出结果为 {"a":2,"b":3}
     *
     * @param convert 指定的JsonConvert
     * @param objs    输出对象
     */
    public void finishMapJson(final JsonConvert convert, final Object... objs) {
        this.contentType = this.jsonContentType;
        if (this.recycleListener != null) this.output = objs;
        finish(convert.convertMapTo(getBodyBufferSupplier(), objs));
    }

    /**
     * 将对象以JSON格式输出
     *
     * @param type 指定的类型
     * @param obj  输出对象
     */
    public void finishJson(final Type type, final Object obj) {
        this.contentType = this.jsonContentType;
        this.output = obj;
        finish(request.getJsonConvert().convertTo(getBodyBufferSupplier(), type, obj));
    }

    /**
     * 将对象以JSON格式输出
     *
     * @param convert 指定的JsonConvert
     * @param type    指定的类型
     * @param obj     输出对象
     */
    public void finishJson(final JsonConvert convert, final Type type, final Object obj) {
        this.contentType = this.jsonContentType;
        if (this.recycleListener != null) this.output = obj;
        finish(convert.convertTo(getBodyBufferSupplier(), type, obj));
    }

    /**
     * 将对象以JSON格式输出
     *
     * @param objs 输出对象
     */
    public void finishJson(final Object... objs) {
        this.contentType = this.jsonContentType;
        if (this.recycleListener != null) this.output = objs;
        finish(request.getJsonConvert().convertTo(getBodyBufferSupplier(), objs));
    }

    /**
     * 将RetResult对象以JSON格式输出
     *
     * @param ret RetResult输出对象
     */
    public void finishJson(final org.redkale.service.RetResult ret) {
        this.contentType = this.jsonContentType;
        if (this.recycleListener != null) this.output = ret;
        if (ret != null && !ret.isSuccess()) {
            this.header.addValue("retcode", String.valueOf(ret.getRetcode()));
            this.header.addValue("retinfo", ret.getRetinfo());
        }
        finish(request.getJsonConvert().convertTo(getBodyBufferSupplier(), ret));
    }

    /**
     * 将RetResult对象以JSON格式输出
     *
     * @param convert 指定的JsonConvert
     * @param ret     RetResult输出对象
     */
    public void finishJson(final JsonConvert convert, final org.redkale.service.RetResult ret) {
        this.contentType = this.jsonContentType;
        if (this.recycleListener != null) this.output = ret;
        if (ret != null && !ret.isSuccess()) {
            this.header.addValue("retcode", String.valueOf(ret.getRetcode()));
            this.header.addValue("retinfo", ret.getRetinfo());
        }
        finish(convert.convertTo(getBodyBufferSupplier(), ret));
    }

    /**
     * 将CompletableFuture的结果对象以JSON格式输出
     *
     * @param future 输出对象的句柄
     */
    public void finishJson(final CompletableFuture future) {
        finish(request.getJsonConvert(), (Type) null, future);
    }

    /**
     * 将CompletableFuture的结果对象以JSON格式输出
     *
     * @param convert 指定的JsonConvert
     * @param future  输出对象的句柄
     */
    @SuppressWarnings("unchecked")
    public void finishJson(final JsonConvert convert, final CompletableFuture future) {
        finish(convert, (Type) null, future);
    }

    /**
     * 将CompletableFuture的结果对象以JSON格式输出
     *
     * @param convert 指定的JsonConvert
     * @param type    指定的类型
     * @param future  输出对象的句柄
     */
    @SuppressWarnings("unchecked")
    public void finishJson(final JsonConvert convert, final Type type, final CompletableFuture future) {
        finish(convert, type, future);
    }

    /**
     * 将结果对象输出
     *
     * @param obj 输出对象
     */
    @SuppressWarnings("unchecked")
    public void finish(final Object obj) {
        finish(request.getJsonConvert(), (Type) null, obj);
    }

    /**
     * 将结果对象输出
     *
     * @param convert 指定的Convert
     * @param obj     输出对象
     */
    @SuppressWarnings("unchecked")
    public void finish(final Convert convert, final Object obj) {
        finish(convert, (Type) null, obj);
    }

    /**
     * 将结果对象输出
     *
     * @param convert 指定的Convert
     * @param type    指定的类型
     * @param obj     输出对象
     */
    @SuppressWarnings("unchecked")
    public void finish(final Convert convert, final Type type, final Object obj) {
        if (obj == null) {
            finish("null");
        } else if (obj instanceof CompletableFuture) {
            ((CompletableFuture) obj).whenComplete((v, e) -> {
                if (e != null) {
                    context.getLogger().log(Level.WARNING, "Servlet occur, forece to close channel. request = " + request + ", result is CompletableFuture", (Throwable) e);
                    finish(500, null);
                    return;
                }
                finish(convert, type, v);
            });
        } else if (obj instanceof CharSequence) {
            finish((String) obj.toString());
        } else if (obj instanceof byte[]) {
            finish((byte[]) obj);
        } else if (obj instanceof ByteBuffer) {
            finish((ByteBuffer) obj);
        } else if (obj instanceof ByteBuffer[]) {
            finish((ByteBuffer[]) obj);
        } else if (obj instanceof File) {
            try {
                finish((File) obj);
            } catch (IOException e) {
                context.getLogger().log(Level.WARNING, "HttpServlet finish File occur, forece to close channel. request = " + getRequest(), e);
                finish(500, null);
            }
        } else if (obj instanceof HttpResult) {
            HttpResult result = (HttpResult) obj;
            if (result.getContentType() != null) setContentType(result.getContentType());
            addHeader(result.getHeaders()).addCookie(result.getCookies()).setStatus(result.getStatus() < 1 ? 200 : result.getStatus());
            if (result.getResult() == null) {
                finish("");
            } else if (result.getResult() instanceof CharSequence) {
                finish(result.getResult().toString());
            } else {
                finish(convert, result.getResult());
            }
        } else {
            if (hasRender) {
                if (onlyoneHttpRender != null) {
                    if (onlyoneHttpRender.getType().isAssignableFrom(obj.getClass())) {
                        onlyoneHttpRender.renderTo(this.request, this, convert, obj);
                        return;
                    }
                } else {
                    Class objt = obj.getClass();
                    for (HttpRender render : this.renders) {
                        if (render.getType().isAssignableFrom(objt)) {
                            render.renderTo(this.request, this, convert, obj);
                            return;
                        }
                    }
                }
            }
            if (convert instanceof JsonConvert) {
                this.contentType = this.jsonContentType;
            } else if (convert instanceof TextConvert) {
                this.contentType = this.plainContentType;
            }
            if (this.recycleListener != null) this.output = obj;
            if (obj instanceof org.redkale.service.RetResult) {
                org.redkale.service.RetResult ret = (org.redkale.service.RetResult) obj;
                if (!ret.isSuccess()) {
                    this.header.addValue("retcode", String.valueOf(ret.getRetcode())).addValue("retinfo", ret.getRetinfo());
                }
            }
            ByteBuffer[] buffers = type == null ? convert.convertTo(getBodyBufferSupplier(), obj)
                : convert.convertTo(getBodyBufferSupplier(), type, obj);
            finish(buffers);
        }
    }

    /**
     * 将指定字符串以响应结果输出
     *
     * @param obj 输出内容
     */
    public void finish(String obj) {
        if (isClosed()) return;
        if (this.recycleListener != null) this.output = obj;
        if (obj == null || obj.isEmpty()) {
            this.contentLength = 0;
            final ByteBuffer headbuf = createHeader();
            headbuf.flip();
            super.finish(headbuf);
            return;
        }
        if (context.getCharset() == null) {
            if (bufferHandler != null) {
                bufferHandler.apply(this, new ByteBuffer[]{ByteBuffer.wrap(Utility.encodeUTF8(obj))});
            }
            final char[] chars = Utility.charArray(obj);
            this.contentLength = Utility.encodeUTF8Length(chars);
            final ByteBuffer headbuf = createHeader();
            ByteBuffer buf2 = Utility.encodeUTF8(headbuf, (int) this.contentLength, chars);
            headbuf.flip();
            if (buf2 == null) {
                super.finish(headbuf);
            } else {
                super.finish(headbuf, buf2);
            }
        } else {
            ByteBuffer buffer = context.getCharset().encode(obj);
            if (bufferHandler != null) {
                ByteBuffer[] bufs = bufferHandler.apply(this, new ByteBuffer[]{buffer});
                if (bufs != null) buffer = bufs[0];
            }
            this.contentLength = buffer.remaining();
            final ByteBuffer headbuf = createHeader();
            headbuf.flip();
            super.finish(headbuf, buffer);
        }
    }

    /**
     * 以指定响应码附带内容输出
     *
     * @param status  响应码
     * @param message 输出内容
     */
    public void finish(int status, String message) {
        if (isClosed()) return;
        this.status = status;
        if (status != 200) super.refuseAlive();
        finish(message);
    }

    /**
     * 以304状态码输出
     */
    public void finish304() {
        super.finish(buffer304.duplicate());
    }

    /**
     * 以404状态码输出
     */
    public void finish404() {
        super.finish(buffer404.duplicate());
    }

    /**
     * 将指定byte[]按响应结果输出
     *
     * @param bs 输出内容
     */
    @Override
    public void finish(final byte[] bs) {
        if (isClosed()) return; //避免重复关闭
        if (this.context.getBufferCapacity() >= bs.length) {
            ByteBuffer buffer = this.context.pollBuffer();
            buffer.put(bs);
            buffer.flip();
            this.finish(false, buffer);
        } else {
            this.finish(false, ByteBuffer.wrap(bs));
        }
    }

    /**
     * 将指定ByteBuffer按响应结果输出
     *
     * @param buffer 输出内容
     */
    @Override
    public void finish(ByteBuffer buffer) {
        finish(false, buffer);
    }

    /**
     * 将指定ByteBuffer按响应结果输出
     *
     * @param kill   输出后是否强制关闭连接
     * @param buffer 输出内容
     */
    @Override
    public void finish(boolean kill, ByteBuffer buffer) {
        if (isClosed()) return; //避免重复关闭
        if (!this.headsended) {
            this.contentLength = buffer == null ? 0 : buffer.remaining();
            ByteBuffer headbuf = createHeader();
            headbuf.flip();
            if (buffer == null) {
                super.finish(kill, headbuf);
            } else {
                super.finish(kill, new ByteBuffer[]{headbuf, buffer});
            }
        } else {
            super.finish(kill, buffer);
        }
    }

    /**
     * 将指定ByteBuffer数组按响应结果输出
     *
     * @param buffers 输出内容
     */
    @Override
    public void finish(ByteBuffer... buffers) {
        finish(false, buffers);
    }

    /**
     * 将指定ByteBuffer数组按响应结果输出
     *
     * @param kill    输出后是否强制关闭连接
     * @param buffers 输出内容
     */
    @Override
    public void finish(boolean kill, ByteBuffer... buffers) {
        if (isClosed()) return; //避免重复关闭
        if (bufferHandler != null) {
            ByteBuffer[] bufs = bufferHandler.apply(this, buffers);
            if (bufs != null) buffers = bufs;
        }
        if (kill) refuseAlive();
        if (!this.headsended) {
            long len = 0;
            for (ByteBuffer buf : buffers) {
                len += buf.remaining();
            }
            this.contentLength = len;
            ByteBuffer headbuf = createHeader();
            headbuf.flip();
            if (buffers == null) {
                super.finish(kill, headbuf);
            } else {
                ByteBuffer[] newbuffers = new ByteBuffer[buffers.length + 1];
                newbuffers[0] = headbuf;
                System.arraycopy(buffers, 0, newbuffers, 1, buffers.length);
                super.finish(kill, newbuffers);
            }
        } else {
            super.finish(kill, buffers);
        }
    }

    /**
     * 异步输出指定内容
     *
     * @param <A>        泛型
     * @param buffer     输出内容
     * @param attachment 异步回调参数
     * @param handler    异步回调函数
     */
    public <A> void sendBody(ByteBuffer buffer, A attachment, CompletionHandler<Integer, A> handler) {
        if (!this.headsended) {
            if (this.contentLength < 0) this.contentLength = buffer == null ? 0 : buffer.remaining();
            ByteBuffer headbuf = createHeader();
            headbuf.flip();
            if (buffer == null) {
                super.send(headbuf, attachment, handler);
            } else {
                super.send(new ByteBuffer[]{headbuf, buffer}, attachment, handler);
            }
        } else {
            super.send(buffer, attachment, handler);
        }
    }

    /**
     * 异步输出指定内容
     *
     * @param <A>        泛型
     * @param buffers    输出内容
     * @param attachment 异步回调参数
     * @param handler    异步回调函数
     */
    public <A> void sendBody(ByteBuffer[] buffers, A attachment, CompletionHandler<Integer, A> handler) {
        if (!this.headsended) {
            if (this.contentLength < 0) {
                int len = 0;
                if (buffers != null && buffers.length > 0) {
                    for (ByteBuffer b : buffers) {
                        len += b.remaining();
                    }
                }
                this.contentLength = len;
            }
            ByteBuffer headbuf = createHeader();
            headbuf.flip();
            if (buffers == null || buffers.length == 0) {
                super.send(headbuf, attachment, handler);
            } else {
                super.send(Utility.unshift(buffers, headbuf), attachment, handler);
            }
        } else {
            super.send(buffers, attachment, handler);
        }
    }

    /**
     * 将指定文件按响应结果输出
     *
     * @param file 输出文件
     *
     * @throws IOException IO异常
     */
    public void finish(File file) throws IOException {
        finishFile(null, file, null);
    }

    /**
     * 将文件按指定文件名输出
     *
     * @param filename 输出文件名
     * @param file     输出文件
     *
     * @throws IOException IO异常
     */
    public void finish(final String filename, File file) throws IOException {
        finishFile(filename, file, null);
    }

    /**
     * 将指定文件句柄或文件内容按响应结果输出，若fileBody不为null则只输出fileBody内容
     *
     * @param file     输出文件
     * @param fileBody 文件内容， 没有则输出file
     *
     * @throws IOException IO异常
     */
    protected void finishFile(final File file, ByteBuffer fileBody) throws IOException {
        finishFile(null, file, fileBody);
    }

    /**
     * 将指定文件句柄或文件内容按指定文件名输出，若fileBody不为null则只输出fileBody内容
     * file 与 fileBody 不能同时为空
     * file 与 filename 也不能同时为空
     *
     * @param filename 输出文件名
     * @param file     输出文件
     * @param fileBody 文件内容， 没有则输出file
     *
     * @throws IOException IO异常
     */
    protected void finishFile(final String filename, final File file, ByteBuffer fileBody) throws IOException {
        if ((file == null || !file.isFile() || !file.canRead()) && fileBody == null) {
            finish404();
            return;
        }
        if (fileBody != null) fileBody = fileBody.duplicate().asReadOnlyBuffer();
        final long length = file == null ? fileBody.remaining() : file.length();
        final String match = request.getHeader("If-None-Match");
        final String etag = (file == null ? 0L : file.lastModified()) + "-" + length;
        if (match != null && etag.equals(match)) {
            //finish304();
            //return;
        }
        this.contentLength = length;
        if (filename != null && !filename.isEmpty() && file != null) {
            addHeader("Content-Disposition", "attachment;filename=" + URLEncoder.encode(filename, "UTF-8"));
        }
        this.contentType = MimeType.getByFilename(filename == null || filename.isEmpty() ? file.getName() : filename);
        if (this.contentType == null) this.contentType = "application/octet-stream";
        String range = request.getHeader("Range");
        if (range != null && (!range.startsWith("bytes=") || range.indexOf(',') >= 0)) range = null;
        long start = -1;
        long len = -1;
        if (range != null) {
            range = range.substring("bytes=".length());
            int pos = range.indexOf('-');
            start = pos == 0 ? 0 : Integer.parseInt(range.substring(0, pos));
            long end = (pos == range.length() - 1) ? -1 : Long.parseLong(range.substring(pos + 1));
            long clen = end > 0 ? (end - start + 1) : (length - start);
            this.status = 206;
            addHeader("Accept-Ranges", "bytes");
            addHeader("Content-Range", "bytes " + start + "-" + (end > 0 ? end : length - 1) + "/" + length);
            this.contentLength = clen;
            len = end > 0 ? clen : end;
        }
        this.addHeader("ETag", etag);
        ByteBuffer hbuffer = createHeader();
        hbuffer.flip();
        if (fileBody == null) {
            if (this.recycleListener != null) this.output = file;
            finishFile(hbuffer, file, start, len);
        } else {
            if (start >= 0) {
                fileBody.position((int) start);
                if (len > 0) fileBody.limit((int) (fileBody.position() + len));
            }
            if (this.recycleListener != null) this.output = fileBody;
            super.finish(hbuffer, fileBody);
        }
    }

    private void finishFile(ByteBuffer hbuffer, File file, long offset, long length) throws IOException {
        this.channel.write(hbuffer, hbuffer, new TransferFileHandler(file, offset, length));
    }

    //Header大小不能超过一个ByteBuffer的容量
    protected ByteBuffer createHeader() {
        this.headsended = true;
        ByteBuffer buffer = this.pollWriteReadBuffer();
        if (this.status == 200) {
            buffer.put(status200Bytes);
        } else {
            buffer.put(("HTTP/1.1 " + this.status + " " + httpCodes.get(this.status) + "\r\n").getBytes());
        }
        if (this.contentLength >= 0) buffer.put(("Content-Length: " + this.contentLength + "\r\n").getBytes());
        if (this.contentType == this.jsonContentType) {
            buffer.put(this.jsonContentTypeBytes);
        } else if (this.contentType == null || this.contentType == this.plainContentType) {
            buffer.put(this.plainContentTypeBytes);
        } else {
            buffer.put(("Content-Type: " + (this.contentType == null ? this.plainContentType : this.contentType) + "\r\n").getBytes());
        }
        buffer.put(serverNameBytes);
        buffer.put(("Date: " + RFC_1123_DATE_TIME.format(java.time.ZonedDateTime.now(ZONE_GMT)) + "\r\n").getBytes());
        if (!this.request.isKeepAlive()) buffer.put(connectCloseBytes);

        if (this.defaultAddHeaders != null) {
            for (String[] headers : this.defaultAddHeaders) {
                if (headers.length > 3) {
                    String v = request.getParameter(headers[2]);
                    if (v != null) this.header.addValue(headers[0], v);
                } else if (headers.length > 2) {
                    String v = request.getHeader(headers[2]);
                    if (v != null) this.header.addValue(headers[0], v);
                } else {
                    this.header.addValue(headers[0], headers[1]);
                }
            }
        }
        if (this.defaultSetHeaders != null) {
            for (String[] headers : this.defaultSetHeaders) {
                if (headers.length > 3) {
                    String v = request.getParameter(headers[2]);
                    if (v != null) this.header.setValue(headers[0], v);
                } else if (headers.length > 2) {
                    String v = request.getHeader(headers[2]);
                    if (v != null) this.header.setValue(headers[0], v);
                } else {
                    this.header.setValue(headers[0], headers[1]);
                }
            }
        }
        for (Entry<String> en : this.header.getStringEntrys()) {
            buffer.put((en.name + ": " + en.getValue() + "\r\n").getBytes());
        }
        if (request.newsessionid != null) {
            String domain = defcookie == null ? null : defcookie.getDomain();
            if (domain == null) {
                domain = "";
            } else {
                domain = "Domain=" + domain + "; ";
            }
            String path = defcookie == null ? null : defcookie.getPath();
            if (path == null || path.isEmpty()) path = "/";
            if (request.newsessionid.isEmpty()) {
                buffer.put(("Set-Cookie: " + HttpRequest.SESSIONID_NAME + "=; " + domain + "Path=" + path + "; Max-Age=0; HttpOnly\r\n").getBytes());
            } else {
                buffer.put(("Set-Cookie: " + HttpRequest.SESSIONID_NAME + "=" + request.newsessionid + "; " + domain + "Path=" + path + "; HttpOnly\r\n").getBytes());
            }
        }
        if (this.cookies != null) {
            for (HttpCookie cookie : this.cookies) {
                if (cookie == null) continue;
                if (defcookie != null) {
                    if (defcookie.getDomain() != null && cookie.getDomain() == null) cookie.setDomain(defcookie.getDomain());
                    if (defcookie.getPath() != null && cookie.getPath() == null) cookie.setPath(defcookie.getPath());
                }
                buffer.put(("Set-Cookie: " + genString(cookie) + "\r\n").getBytes());
            }
        }
        buffer.put(LINE);
        return buffer;
    }

    private CharSequence genString(HttpCookie cookie) {
        StringBuilder sb = new StringBuilder();
        sb.append(cookie.getName()).append("=").append(cookie.getValue()).append("; Version=1");
        if (cookie.getDomain() != null) sb.append("; Domain=").append(cookie.getDomain());
        if (cookie.getPath() != null) sb.append("; Path=").append(cookie.getPath());
        if (cookie.getPortlist() != null) sb.append("; Port=").append(cookie.getPortlist());
        if (cookie.getMaxAge() > 0) {
            sb.append("; Max-Age=").append(cookie.getMaxAge());
            sb.append("; Expires=").append(RFC_1123_DATE_TIME.format(java.time.ZonedDateTime.now(ZONE_GMT).plusSeconds(cookie.getMaxAge())));
        }
        if (cookie.getSecure()) sb.append("; Secure");
        if (cookie.isHttpOnly()) sb.append("; HttpOnly");
        return sb;
    }

    /**
     * 跳过header的输出
     * 通常应用场景是，调用者的输出内容里已经包含了HTTP的响应头信息，因此需要调用此方法避免重复输出HTTP响应头信息。
     *
     * @return HttpResponse
     */
    public HttpResponse skipHeader() {
        this.headsended = true;
        return this;
    }

    protected DefaultAnyValue duplicateHeader() {
        return this.header.duplicate();
    }

    /**
     * 设置Header值
     *
     * @param name  header名
     * @param value header值
     *
     * @return HttpResponse
     */
    public HttpResponse setHeader(String name, Object value) {
        this.header.setValue(name, String.valueOf(value));
        return this;
    }

    /**
     * 添加Header值
     *
     * @param name  header名
     * @param value header值
     *
     * @return HttpResponse
     */
    public HttpResponse addHeader(String name, Object value) {
        this.header.addValue(name, String.valueOf(value));
        return this;
    }

    /**
     * 添加Header值
     *
     * @param map header值
     *
     * @return HttpResponse
     */
    public HttpResponse addHeader(Map<String, ?> map) {
        if (map == null || map.isEmpty()) return this;
        for (Map.Entry<String, ?> en : map.entrySet()) {
            this.header.addValue(en.getKey(), String.valueOf(en.getValue()));
        }
        return this;
    }

    /**
     * 设置状态码
     *
     * @param status 状态码
     *
     * @return HttpResponse
     */
    public HttpResponse setStatus(int status) {
        this.status = status;
        return this;
    }

    /**
     * 获取状态码
     *
     * @return 状态码
     */
    public int getStatus() {
        return this.status;
    }

    /**
     * 获取 ContentType
     *
     * @return ContentType
     */
    public String getContentType() {
        return contentType;
    }

    /**
     * 设置 ContentType
     *
     * @param contentType ContentType
     *
     * @return HttpResponse
     */
    public HttpResponse setContentType(String contentType) {
        this.contentType = contentType;
        return this;
    }

    /**
     * 获取内容长度
     *
     * @return 内容长度
     */
    public long getContentLength() {
        return contentLength;
    }

    /**
     * 设置内容长度
     *
     * @param contentLength 内容长度
     *
     * @return HttpResponse
     */
    public HttpResponse setContentLength(long contentLength) {
        this.contentLength = contentLength;
        return this;
    }

    /**
     * 获取输出时的拦截器
     *
     * @return 拦截器
     */
    protected BiFunction<HttpResponse, ByteBuffer[], ByteBuffer[]> getBufferHandler() {
        return bufferHandler;
    }

    /**
     * 设置输出时的拦截器
     *
     * @param bufferHandler 拦截器
     */
    protected void setBufferHandler(BiFunction<HttpResponse, ByteBuffer[], ByteBuffer[]> bufferHandler) {
        this.bufferHandler = bufferHandler;
    }

    protected final class TransferFileHandler implements CompletionHandler<Integer, ByteBuffer> {

        private final File file;

        private final AsynchronousFileChannel filechannel;

        private final long max; //需要读取的字节数， -1表示读到文件结尾

        private long count;//读取文件的字节数

        private long readpos = 0;

        private boolean hdwrite = true; //写入Header

        private boolean read = false;

        public TransferFileHandler(File file) throws IOException {
            this.file = file;
            this.filechannel = AsynchronousFileChannel.open(file.toPath(), options, ((HttpContext) context).getExecutor());
            this.readpos = 0;
            this.max = file.length();
        }

        public TransferFileHandler(File file, long offset, long len) throws IOException {
            this.file = file;
            this.filechannel = AsynchronousFileChannel.open(file.toPath(), options, ((HttpContext) context).getExecutor());
            this.readpos = offset <= 0 ? 0 : offset;
            this.max = len <= 0 ? file.length() : len;
        }

        @Override
        public void completed(Integer result, ByteBuffer attachment) {
            //(Utility.now() + "---" + Thread.currentThread().getName() + "-----------" + file + "-------------------result: " + result + ", max = " + max + ", readpos = " + readpos + ", count = " + count + ", " + (hdwrite ? "正在写Header" : (read ? "准备读" : "准备写")));
            if (result < 0 || count >= max) {
                failed(null, attachment);
                return;
            }
            if (hdwrite && attachment.hasRemaining()) { //Header还没写完
                channel.write(attachment, attachment, this);
                return;
            }
            if (hdwrite) {
                //(Utility.now() + "---" + Thread.currentThread().getName() + "-----------" + file + "-------------------Header写入完毕， 准备读取文件.");
                hdwrite = false;
                read = true;
                result = 0;
            }
            if (read) {
                count += result;
            } else {
                readpos += result;
            }
            if (read && attachment.hasRemaining()) { //Buffer还没写完
                channel.write(attachment, attachment, this);
                return;
            }

            if (read) {
                read = false;
                attachment.clear();
                filechannel.read(attachment, readpos, attachment, this);
            } else {
                read = true;
                if (count > max) {
                    attachment.limit((int) (attachment.position() + max - count));
                }
                attachment.flip();
                if (attachment.hasRemaining()) {
                    channel.write(attachment, attachment, this);
                } else {
                    failed(null, attachment);
                }
            }
        }

        @Override
        public void failed(Throwable exc, ByteBuffer attachment) {
            context.offerBuffer(attachment);
            finish(true);
            try {
                filechannel.close();
            } catch (IOException e) {
            }
        }

    }
}
