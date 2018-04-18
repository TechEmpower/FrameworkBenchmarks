/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import static org.redkale.net.http.WebSocketServlet.DEFAILT_LIVEINTERVAL;
import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.util.function.*;
import java.util.logging.*;
import java.util.stream.*;
import org.redkale.convert.Convert;
import org.redkale.net.Cryptor;
import static org.redkale.net.http.WebSocket.RETCODE_GROUP_EMPTY;
import static org.redkale.net.http.WebSocketServlet.*;
import org.redkale.util.*;

/**
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class WebSocketEngine {

    @Comment("全局自增长ID, 为了确保在一个进程里多个WebSocketEngine定时发送ping时不会同时进行")
    private static final AtomicInteger sequence = new AtomicInteger();

    @Comment("Engine自增长序号ID")
    private final int index;

    @Comment("当前WebSocket对应的Engine")
    private final String engineid;

    @Comment("当前WebSocket对应的Node")
    protected final WebSocketNode node;

    //HttpContext
    protected final HttpContext context;

    //Convert
    protected final Convert sendConvert;

    @Comment("是否单用户单连接")
    protected final boolean single;

    @Comment("在线用户ID对应的WebSocket组，用于单用户单连接模式")
    private final Map<Serializable, WebSocket> websockets = new ConcurrentHashMap<>();

    @Comment("在线用户ID对应的WebSocket组，用于单用户多连接模式")
    private final Map<Serializable, List<WebSocket>> websockets2 = new ConcurrentHashMap<>();

    @Comment("当前连接数")
    protected final AtomicInteger currconns = new AtomicInteger();

    @Comment("用于PING的定时器")
    private ScheduledThreadPoolExecutor scheduler;

    @Comment("日志")
    protected final Logger logger;

    @Comment("PING的间隔秒数")
    protected int liveinterval;

    @Comment("最大连接数, 为0表示无限制")
    protected int wsmaxconns;

    @Comment("最大消息体长度, 小于1表示无限制")
    protected int wsmaxbody;

    @Comment("加密解密器")
    protected Cryptor cryptor;

    protected WebSocketEngine(String engineid, boolean single, HttpContext context, int liveinterval,
        int wsmaxconns, int wsmaxbody, Cryptor cryptor, WebSocketNode node, Convert sendConvert, Logger logger) {
        this.engineid = engineid;
        this.single = single;
        this.context = context;
        this.sendConvert = sendConvert;
        this.node = node;
        this.liveinterval = liveinterval;
        this.wsmaxconns = wsmaxconns;
        this.wsmaxbody = wsmaxbody;
        this.cryptor = cryptor;
        this.logger = logger;
        this.index = sequence.getAndIncrement();
    }

    void init(AnyValue conf) {
        AnyValue props = conf;
        if (conf != null && conf.getAnyValue("properties") != null) props = conf.getAnyValue("properties");
        this.liveinterval = props == null ? (liveinterval < 0 ? DEFAILT_LIVEINTERVAL : liveinterval) : props.getIntValue(WEBPARAM__LIVEINTERVAL, (liveinterval < 0 ? DEFAILT_LIVEINTERVAL : liveinterval));
        if (liveinterval <= 0) return;
        if (props != null) this.wsmaxconns = props.getIntValue(WEBPARAM__WSMAXCONNS, this.wsmaxconns);
        if (props != null) this.wsmaxbody = props.getIntValue(WEBPARAM__WSMAXBODY, this.wsmaxbody);
        if (scheduler != null) return;
        this.scheduler = new ScheduledThreadPoolExecutor(1, (Runnable r) -> {
            final Thread t = new Thread(r, engineid + "-WebSocket-LiveInterval-Thread");
            t.setDaemon(true);
            return t;
        });
        long delay = (liveinterval - System.currentTimeMillis() / 1000 % liveinterval) + index * 5;
        final int intervalms = liveinterval * 1000;
        scheduler.scheduleWithFixedDelay(() -> {
            long now = System.currentTimeMillis();
            getLocalWebSockets().stream().filter(x -> (now - x.getLastReadTime()) > intervalms).forEach(x -> x.sendPing());
        }, delay, liveinterval, TimeUnit.SECONDS);
        if (logger.isLoggable(Level.FINEST)) logger.finest(this.getClass().getSimpleName() + "(" + engineid + ")" + " start keeplive(wsmaxconns:" + wsmaxconns + ", delay:" + delay + "s, interval:" + liveinterval + "s) scheduler executor");
    }

    void destroy(AnyValue conf) {
        if (scheduler != null) scheduler.shutdownNow();
    }

    @Comment("添加WebSocket")
    void add(WebSocket socket) {
        if (single) {
            currconns.incrementAndGet();
            websockets.put(socket._userid, socket);
        } else { //非线程安全， 在常规场景中无需锁
            List<WebSocket> list = websockets2.get(socket._userid);
            if (list == null) {
                list = new CopyOnWriteArrayList<>();
                websockets2.put(socket._userid, list);
            }
            currconns.incrementAndGet();
            list.add(socket);
        }
        if (node != null) node.connect(socket._userid);
    }

    @Comment("从WebSocketEngine删除指定WebSocket")
    void remove(WebSocket socket) {
        Serializable userid = socket._userid;
        if (single) {
            currconns.decrementAndGet();
            websockets.remove(userid);
            if (node != null) node.disconnect(userid);
        } else { //非线程安全， 在常规场景中无需锁
            List<WebSocket> list = websockets2.get(userid);
            if (list != null) {
                currconns.decrementAndGet();
                list.remove(socket);
                if (list.isEmpty()) {
                    websockets2.remove(userid);
                    if (node != null) node.disconnect(userid);
                }
            }
        }
    }

    @Comment("更改WebSocket的userid")
    CompletableFuture<Void> changeUserid(WebSocket socket, final Serializable newuserid) {
        if (newuserid == null) throw new NullPointerException("newuserid is null");
        final Serializable olduserid = socket._userid;
        socket._userid = newuserid;
        if (single) {
            websockets.remove(olduserid);
            websockets.put(newuserid, socket);
        } else { //非线程安全， 在常规场景中无需锁
            List<WebSocket> oldlist = websockets2.get(olduserid);
            if (oldlist != null) {
                oldlist.remove(socket);
                if (oldlist.isEmpty()) websockets2.remove(olduserid);
            }
            List<WebSocket> newlist = websockets2.get(newuserid);
            if (newlist == null) {
                newlist = new CopyOnWriteArrayList<>();
                websockets2.put(newuserid, newlist);
            }
            newlist.add(socket);
        }
        if (node != null) return node.changeUserid(olduserid, newuserid);
        return CompletableFuture.completedFuture(null);
    }

    @Comment("强制关闭本地用户的WebSocket")
    public int forceCloseLocalWebSocket(Serializable userid) {
        if (single) {
            WebSocket ws = websockets.get(userid);
            if (ws == null) return 0;
            ws.close();
            return 1;
        }
        List<WebSocket> list = websockets2.get(userid);
        if (list == null || list.isEmpty()) return 0;
        List<WebSocket> list2 = new ArrayList<>(list);
        for (WebSocket ws : list2) {
            ws.close();
        }
        return list2.size();
    }

    @Comment("给所有连接用户发送消息")
    public CompletableFuture<Integer> broadcastMessage(final Object message, final boolean last) {
        return broadcastMessage((Predicate) null, message, last);
    }

    @Comment("给指定WebSocket连接用户发送消息")
    public CompletableFuture<Integer> broadcastMessage(final WebSocketRange wsrange, final Object message, final boolean last) {
        Predicate<WebSocket> predicate = wsrange == null ? null : (ws) -> ws.predicate(wsrange);
        return broadcastMessage(predicate, message, last);
    }

    @Comment("给指定WebSocket连接用户发送消息")
    public CompletableFuture<Integer> broadcastMessage(final Predicate<WebSocket> predicate, final Object message, final boolean last) {
        if (message instanceof CompletableFuture) {
            return ((CompletableFuture) message).thenCompose((json) -> broadcastMessage(predicate, json, last));
        }
        final boolean more = (!(message instanceof WebSocketPacket) || ((WebSocketPacket) message).sendBuffers == null);
        if (more) {
            //此处的WebSocketPacket只能是包含payload或bytes内容的，不能包含sendConvert、sendJson、sendBuffers
            final WebSocketPacket packet = (message instanceof WebSocketPacket) ? (WebSocketPacket) message
                : ((message == null || message instanceof CharSequence || message instanceof byte[])
                    ? new WebSocketPacket((Serializable) message, last) : new WebSocketPacket(this.sendConvert, false, message, last));
            packet.setSendBuffers(packet.encode(context.getBufferSupplier(), context.getBufferConsumer(), cryptor));
            CompletableFuture<Integer> future = null;
            if (single) {
                for (WebSocket websocket : websockets.values()) {
                    if (predicate != null && !predicate.test(websocket)) continue;
                    future = future == null ? websocket.sendPacket(packet) : future.thenCombine(websocket.sendPacket(packet), (a, b) -> a | (Integer) b);
                }
            } else {
                for (List<WebSocket> list : websockets2.values()) {
                    for (WebSocket websocket : list) {
                        if (predicate != null && !predicate.test(websocket)) continue;
                        future = future == null ? websocket.sendPacket(packet) : future.thenCombine(websocket.sendPacket(packet), (a, b) -> a | (Integer) b);
                    }
                }
            }
            if (future != null) future = future.whenComplete((rs, ex) -> context.offerBuffer(packet.sendBuffers));
            return future == null ? CompletableFuture.completedFuture(RETCODE_GROUP_EMPTY) : future;
        } else {
            CompletableFuture<Integer> future = null;
            if (single) {
                for (WebSocket websocket : websockets.values()) {
                    if (predicate != null && !predicate.test(websocket)) continue;
                    future = future == null ? websocket.send(message, last) : future.thenCombine(websocket.send(message, last), (a, b) -> a | (Integer) b);
                }
            } else {
                for (List<WebSocket> list : websockets2.values()) {
                    for (WebSocket websocket : list) {
                        if (predicate != null && !predicate.test(websocket)) continue;
                        future = future == null ? websocket.send(message, last) : future.thenCombine(websocket.send(message, last), (a, b) -> a | (Integer) b);
                    }
                }
            }
            return future == null ? CompletableFuture.completedFuture(RETCODE_GROUP_EMPTY) : future;
        }
    }

    @Comment("给指定用户组发送消息")
    public CompletableFuture<Integer> sendMessage(final Object message, final boolean last, final Stream<? extends Serializable> userids) {
        Object[] array = userids.toArray();
        Serializable[] ss = new Serializable[array.length];
        for (int i = 0; i < array.length; i++) {
            ss[i] = (Serializable) array[i];
        }
        return sendMessage(message, last, ss);
    }

    @Comment("给指定用户组发送消息")
    public CompletableFuture<Integer> sendMessage(final Object message, final boolean last, final Serializable... userids) {
        if (message instanceof CompletableFuture) {
            return ((CompletableFuture) message).thenCompose((json) -> sendMessage(json, last, userids));
        }
        final boolean more = (!(message instanceof WebSocketPacket) || ((WebSocketPacket) message).sendBuffers == null) && userids.length > 1;
        if (more) {
            //此处的WebSocketPacket只能是包含payload或bytes内容的，不能包含sendConvert、sendJson、sendBuffers
            final WebSocketPacket packet = (message instanceof WebSocketPacket) ? (WebSocketPacket) message
                : ((message == null || message instanceof CharSequence || message instanceof byte[])
                    ? new WebSocketPacket((Serializable) message, last) : new WebSocketPacket(this.sendConvert, false, message, last));
            packet.setSendBuffers(packet.encode(context.getBufferSupplier(), context.getBufferConsumer(), cryptor));
            CompletableFuture<Integer> future = null;
            if (single) {
                for (Serializable userid : userids) {
                    WebSocket websocket = websockets.get(userid);
                    if (websocket == null) continue;
                    future = future == null ? websocket.sendPacket(packet) : future.thenCombine(websocket.sendPacket(packet), (a, b) -> a | (Integer) b);
                }
            } else {
                for (Serializable userid : userids) {
                    List<WebSocket> list = websockets2.get(userid);
                    if (list == null) continue;
                    for (WebSocket websocket : list) {
                        future = future == null ? websocket.sendPacket(packet) : future.thenCombine(websocket.sendPacket(packet), (a, b) -> a | (Integer) b);
                    }
                }
            }
            if (future != null) future = future.whenComplete((rs, ex) -> context.offerBuffer(packet.sendBuffers));
            return future == null ? CompletableFuture.completedFuture(RETCODE_GROUP_EMPTY) : future;
        } else {
            CompletableFuture<Integer> future = null;
            if (single) {
                for (Serializable userid : userids) {
                    WebSocket websocket = websockets.get(userid);
                    if (websocket == null) continue;
                    future = future == null ? websocket.send(message, last) : future.thenCombine(websocket.send(message, last), (a, b) -> a | (Integer) b);
                }
            } else {
                for (Serializable userid : userids) {
                    List<WebSocket> list = websockets2.get(userid);
                    if (list == null) continue;
                    for (WebSocket websocket : list) {
                        future = future == null ? websocket.send(message, last) : future.thenCombine(websocket.send(message, last), (a, b) -> a | (Integer) b);
                    }
                }
            }
            return future == null ? CompletableFuture.completedFuture(RETCODE_GROUP_EMPTY) : future;
        }
    }

    @Comment("获取最大连接数")
    public int getLocalWsmaxconns() {
        return this.wsmaxconns;
    }

    @Comment("连接数是否达到上限")
    public boolean isLocalConnLimited() {
        if (this.wsmaxconns < 1) return false;
        return currconns.get() >= this.wsmaxconns;
    }

    @Comment("获取所有连接")
    public Collection<WebSocket> getLocalWebSockets() {
        if (single) return websockets.values();
        List<WebSocket> list = new ArrayList<>();
        websockets2.values().forEach(x -> list.addAll(x));
        return list;
    }

    @Comment("获取所有连接")
    public void forEachLocalWebSocket(Consumer<WebSocket> consumer) {
        if (consumer == null) return;
        if (single) {
            websockets.values().stream().forEach(consumer);
        } else {
            websockets2.values().forEach(x -> x.stream().forEach(consumer));
        }
    }

    @Comment("获取当前连接总数")
    public int getLocalWebSocketSize() {
        if (single) return websockets.size();
        return (int) websockets2.values().stream().mapToInt(sublist -> sublist.size()).count();
    }

    @Comment("获取当前用户总数")
    public int getLocalUserSize() {
        return single ? websockets.size() : websockets2.size();
    }

    @Comment("适用于单用户单连接模式")
    public WebSocket findLocalWebSocket(Serializable userid) {
        if (single) return websockets.get(userid);
        List<WebSocket> list = websockets2.get(userid);
        return (list == null || list.isEmpty()) ? null : list.get(list.size() - 1);
    }

    @Comment("适用于单用户多连接模式")
    public Stream<WebSocket> getLocalWebSockets(Serializable userid) {
        if (single) {
            WebSocket websocket = websockets.get(userid);
            return websocket == null ? Stream.empty() : Stream.of(websocket);
        } else {
            List<WebSocket> list = websockets2.get(userid);
            return list == null ? Stream.empty() : list.stream();
        }
    }

    public boolean existsLocalWebSocket(Serializable userid) {
        return single ? websockets.containsKey(userid) : websockets2.containsKey(userid);
    }

    public String getEngineid() {
        return engineid;
    }
}
