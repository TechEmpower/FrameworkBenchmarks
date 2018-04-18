/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.nio.*;
import java.nio.channels.CompletionHandler;
import java.security.*;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiConsumer;
import java.util.logging.*;
import javax.annotation.*;
import org.redkale.convert.Convert;
import org.redkale.net.Cryptor;
import org.redkale.service.*;
import org.redkale.util.*;

/**
 * <blockquote><pre>
 * 当WebSocketServlet接收一个TCP连接后，进行协议判断，如果成功就会创建一个WebSocket。
 *
 *                                    WebSocketServlet
 *                                            |
 *                                            |
 *                                   WebSocketEngine
 *                                    WebSocketNode
 *                                   /             \
 *                                 /                \
 *                               /                   \
 *                     WebSocket1                 WebSocket2
 *
 * </pre></blockquote>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public abstract class WebSocketServlet extends HttpServlet implements Resourcable {

    @Comment("WebScoket服务器给客户端进行ping操作的间隔时间, 单位: 秒")
    public static final String WEBPARAM__LIVEINTERVAL = "liveinterval";

    @Comment("WebScoket服务器最大连接数，为0表示无限制")
    public static final String WEBPARAM__WSMAXCONNS = "wsmaxconns";

    @Comment("最大消息体长度, 小于1表示无限制")
    public static final String WEBPARAM__WSMAXBODY = "wsmaxbody";

    @Comment("加密解密器")
    public static final String WEBPARAM__CRYPTOR = "cryptor";

    @Comment("WebScoket服务器给客户端进行ping操作的默认间隔时间, 单位: 秒")
    public static final int DEFAILT_LIVEINTERVAL = 15;

    protected final Logger logger = Logger.getLogger(this.getClass().getSimpleName());

    private final MessageDigest digest = getMessageDigest();

    private final BiConsumer<WebSocket, Object> restMessageConsumer = createRestOnMessageConsumer();

    protected Type messageTextType;  //RestWebSocket时会被修改

    //同RestWebSocket.single
    protected boolean single = true; //是否单用户单连接

    //同RestWebSocket.liveinterval
    protected int liveinterval = DEFAILT_LIVEINTERVAL;

    //同RestWebSocket.wsmaxconns
    protected int wsmaxconns = 0;

    //同RestWebSocket.wsmaxbody
    protected int wsmaxbody = 32 * 1024;

    //同RestWebSocket.anyuser
    protected boolean anyuser = false;

    //同RestWebSocket.cryptor, 变量名不可改， 被Rest.createRestWebSocketServlet用到
    protected Cryptor cryptor;

    @Resource(name = "jsonconvert")
    protected Convert jsonConvert;

    @Resource(name = "$_textconvert")
    protected Convert textConvert;

    @Resource(name = "$_binaryconvert")
    protected Convert binaryConvert;

    @Resource(name = "$_sendconvert")
    protected Convert sendConvert;

    @Resource(name = "$")
    protected WebSocketNode node;

    protected WebSocketServlet() {
        Type msgtype = String.class;
        try {
            for (Method method : this.getClass().getDeclaredMethods()) {
                if (!method.getName().equals("createWebSocket")) continue;
                if (method.getParameterCount() > 0) continue;
                Type rt = TypeToken.getGenericType(method.getGenericReturnType(), this.getClass());
                if (rt instanceof ParameterizedType) {
                    msgtype = ((ParameterizedType) rt).getActualTypeArguments()[1];
                }
                if (msgtype == Object.class) msgtype = String.class;
                break;
            }
        } catch (Exception e) {
            logger.warning(this.getClass().getName() + " not designate text message type on createWebSocket Method");
        }
        this.messageTextType = msgtype;
    }

    @Override
    final void preInit(HttpContext context, AnyValue conf) {
        if (this.textConvert == null) this.textConvert = jsonConvert;
        if (this.binaryConvert == null) this.binaryConvert = jsonConvert;
        if (this.sendConvert == null) this.sendConvert = jsonConvert;
        InetSocketAddress addr = context.getServerAddress();
        if (this.node == null) this.node = createWebSocketNode();
        if (this.node == null) {  //没有部署SNCP，即不是分布式
            this.node = new WebSocketNodeService();
            if (logger.isLoggable(Level.WARNING)) logger.warning("Not found WebSocketNode, create a default value for " + getClass().getName());
        }
        if (this.node.sendConvert == null) this.node.sendConvert = this.sendConvert;
        {
            AnyValue props = conf;
            if (conf != null && conf.getAnyValue("properties") != null) props = conf.getAnyValue("properties");
            if (props != null) {
                String cryptorClass = props.getValue(WEBPARAM__CRYPTOR);
                if (cryptorClass != null && !cryptorClass.isEmpty()) {
                    try {
                        this.cryptor = (Cryptor) Thread.currentThread().getContextClassLoader().loadClass(cryptorClass).getDeclaredConstructor().newInstance();
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }
            }
        }
        //存在WebSocketServlet，则此WebSocketNode必须是本地模式Service
        this.node.localEngine = new WebSocketEngine("WebSocketEngine-" + addr.getHostString() + ":" + addr.getPort() + "-[" + resourceName() + "]",
            this.single, context, liveinterval, wsmaxconns, wsmaxbody, this.cryptor, this.node, this.sendConvert, logger);
        this.node.init(conf);
        this.node.localEngine.init(conf);

    }

    @Override
    final void postDestroy(HttpContext context, AnyValue conf) {
        this.node.postDestroy(conf);
        super.destroy(context, conf);
        this.node.localEngine.destroy(conf);
    }

    @Override
    public String resourceName() {
        return this.getClass().getSimpleName().replace("_Dyn", "").toLowerCase().replaceAll("websocket.*$", "").replaceAll("servlet.*$", "");
    }

    @Override
    public final void execute(final HttpRequest request, final HttpResponse response) throws IOException {
        final boolean debug = logger.isLoggable(Level.FINEST);
        if (!request.isWebSocket()) {
            if (debug) logger.finest("WebSocket connect abort, (Not GET Method) or (Connection != Upgrade) or (Upgrade != websocket). request=" + request);
            response.finish(true);
            return;
        }
        final String key = request.getHeader("Sec-WebSocket-Key");
        if (key == null) {
            if (debug) logger.finest("WebSocket connect abort, Not found Sec-WebSocket-Key header. request=" + request);
            response.finish(true);
            return;
        }
        if (this.node.localEngine.isLocalConnLimited()) {
            if (debug) logger.finest("WebSocket connections limit, wsmaxconns=" + this.node.localEngine.getLocalWsmaxconns());
            response.finish(true);
            return;
        }
        final WebSocket webSocket = this.createWebSocket();
        webSocket._engine = this.node.localEngine;
        webSocket._messageTextType = this.messageTextType;
        webSocket._textConvert = textConvert;
        webSocket._binaryConvert = binaryConvert;
        webSocket._sendConvert = sendConvert;
        webSocket._remoteAddress = request.getRemoteAddress();
        webSocket._remoteAddr = request.getRemoteAddr();
        initRestWebSocket(webSocket);
        CompletableFuture<String> sessionFuture = webSocket.onOpen(request);
        if (sessionFuture == null) {
            if (debug) logger.finest("WebSocket connect abort, Not found sessionid. request=" + request);
            response.finish(true);
            return;
        }
        sessionFuture.whenComplete((sessionid, ex) -> {
            if ((sessionid == null && webSocket.delayPackets == null) || ex != null) {
                if (debug || ex != null) logger.log(ex == null ? Level.FINEST : Level.FINE, "WebSocket connect abort, Not found sessionid or occur error. request=" + request, ex);
                response.finish(true);
                return;
            }
            webSocket._sessionid = sessionid;
            request.setKeepAlive(true);
            byte[] bytes = (key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").getBytes();
            synchronized (digest) {
                bytes = digest.digest(bytes);
            }
            response.setStatus(101);
            response.setHeader("Connection", "Upgrade");
            response.addHeader("Upgrade", "websocket");
            response.addHeader("Sec-WebSocket-Accept", Base64.getEncoder().encodeToString(bytes));
            response.sendBody((ByteBuffer) null, null, new CompletionHandler<Integer, Void>() {

                WebSocketRunner temprunner = null;

                @Override
                public void completed(Integer result, Void attachment) {
                    HttpContext context = response.getContext();

                    Runnable createUseridHandler = () -> {
                        CompletableFuture<Serializable> userFuture = webSocket.createUserid();
                        if (userFuture == null) {
                            if (debug) logger.finest("WebSocket connect abort, Create userid abort. request = " + request);
                            response.finish(true);
                            return;
                        }
                        userFuture.whenComplete((userid, ex2) -> {
                            if ((userid == null && webSocket.delayPackets == null) || ex2 != null) {
                                if (debug || ex2 != null) logger.log(ex2 == null ? Level.FINEST : Level.FINE, "WebSocket connect abort, Create userid abort. request = " + request, ex2);
                                response.finish(true);
                                return;
                            }
                            Runnable runHandler = () -> {
                                temprunner = null;
                                webSocket._userid = userid;
                                if (single && !anyuser) {
                                    WebSocketServlet.this.node.existsWebSocket(userid).whenComplete((rs, nex) -> {
                                        if (rs) webSocket.onSingleRepeatConnect();
                                        WebSocketServlet.this.node.localEngine.add(webSocket);
                                        WebSocketRunner runner = new WebSocketRunner(context, webSocket, restMessageConsumer, response.removeChannel());
                                        webSocket._runner = runner;
                                        context.runAsync(runner);
                                        response.finish(true);
                                    });
                                } else {
                                    WebSocketServlet.this.node.localEngine.add(webSocket);
                                    WebSocketRunner runner = new WebSocketRunner(context, webSocket, restMessageConsumer, response.removeChannel());
                                    webSocket._runner = runner;
                                    context.runAsync(runner);
                                    response.finish(true);
                                }
                            };
                            if (webSocket.delayPackets != null) { //存在待发送的消息
                                if (temprunner == null) temprunner = new WebSocketRunner(context, webSocket, restMessageConsumer, response.getChannel());
                                List<WebSocketPacket> delayPackets = webSocket.delayPackets;
                                webSocket.delayPackets = null;
                                CompletableFuture<Integer> cf = null;
                                for (WebSocketPacket packet : delayPackets) {
                                    if (cf == null) {
                                        cf = temprunner.sendMessage(packet);
                                    } else {
                                        cf = cf.thenCombine(temprunner.sendMessage(packet), (a, b) -> a | b);
                                    }
                                }
                                cf.whenComplete((Integer v, Throwable t) -> {
                                    if (userid == null || t != null || (temprunner != null && temprunner.isClosed())) {
                                        if (t != null) logger.log(Level.FINEST, "WebSocket connect abort, Response send delayPackets abort. request = " + request, t);
                                        response.finish(true);
                                    } else {
                                        runHandler.run();
                                    }
                                });
                            } else {
                                runHandler.run();
                            }
                        });
                    };
                    if (webSocket.delayPackets != null) { //存在待发送的消息
                        if (temprunner == null) temprunner = new WebSocketRunner(context, webSocket, restMessageConsumer, response.getChannel());
                        List<WebSocketPacket> delayPackets = webSocket.delayPackets;
                        webSocket.delayPackets = null;
                        CompletableFuture<Integer> cf = null;
                        for (WebSocketPacket packet : delayPackets) {
                            if (cf == null) {
                                cf = temprunner.sendMessage(packet);
                            } else {
                                cf = cf.thenCombine(temprunner.sendMessage(packet), (a, b) -> a | b);
                            }
                        }
                        cf.whenComplete((Integer v, Throwable t) -> {
                            if (sessionid == null || t != null || (temprunner != null && temprunner.isClosed())) {
                                if (t != null) logger.log(Level.FINEST, "WebSocket connect abort, Response send delayPackets abort. request = " + request, t);
                                response.finish(true);
                            } else {
                                createUseridHandler.run();
                            }
                        });
                    } else {
                        createUseridHandler.run();
                    }
                }

                @Override
                public void failed(Throwable exc, Void attachment) {
                    logger.log(Level.FINEST, "WebSocket connect abort, Response send abort. request = " + request, exc);
                    response.finish(true);
                }
            });
        });
    }

    protected abstract <G extends Serializable, T> WebSocket<G, T> createWebSocket();

    protected WebSocketNode createWebSocketNode() {
        return null;
    }

    protected void initRestWebSocket(WebSocket websocket) { //设置WebSocket中的@Resource资源
    }

    protected BiConsumer<WebSocket, Object> createRestOnMessageConsumer() {
        return null;
    }

    private static MessageDigest getMessageDigest() {
        try {
            return MessageDigest.getInstance("SHA-1");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

}
