/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.service;

import static org.redkale.net.http.WebSocket.*;
import java.io.*;
import java.net.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.logging.Level;
import org.redkale.net.WorkThread;
import org.redkale.net.http.*;
import org.redkale.util.*;

/**
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@AutoLoad(false)
@ResourceType(WebSocketNode.class)
public class WebSocketNodeService extends WebSocketNode implements Service {

    @Override
    public void init(AnyValue conf) {
        super.init(conf);
    }

    @Override
    public void destroy(AnyValue conf) {
        super.destroy(conf);
    }

    @Override
    public CompletableFuture<List<String>> getWebSocketAddresses(final @RpcTargetAddress InetSocketAddress targetAddress, final Serializable groupid) {
        if (localSncpAddress == null || !localSncpAddress.equals(targetAddress)) return remoteWebSocketAddresses(targetAddress, groupid);
        if (this.localEngine == null) return CompletableFuture.completedFuture(new ArrayList<>());

        ExecutorService executor = null;
        Thread thread = Thread.currentThread();
        if (thread instanceof WorkThread) {
            executor = ((WorkThread) thread).getExecutor();
        }
        if (executor == null) executor = ForkJoinPool.commonPool();
        
        return CompletableFuture.supplyAsync(() -> {
            final List<String> rs = new ArrayList<>();
            this.localEngine.getLocalWebSockets(groupid).forEach(x -> rs.add(x.getRemoteAddr()));
            return rs;
        }, executor);
    }

    @Override
    public CompletableFuture<Integer> sendMessage(@RpcTargetAddress InetSocketAddress addr, Object message, boolean last, Serializable userid) {
        if (this.localEngine == null) return CompletableFuture.completedFuture(RETCODE_GROUP_EMPTY);
        return this.localEngine.sendMessage(message, last, userid);
    }

    @Override
    public CompletableFuture<Integer> broadcastMessage(@RpcTargetAddress InetSocketAddress addr, final WebSocketRange wsrange, Object message, boolean last) {
        if (this.localEngine == null) return CompletableFuture.completedFuture(RETCODE_GROUP_EMPTY);
        return this.localEngine.broadcastMessage(wsrange, message, last);
    }

    /**
     * 当用户连接到节点，需要更新到CacheSource
     *
     * @param userid   Serializable
     * @param sncpAddr InetSocketAddress
     *
     * @return 无返回值
     */
    @Override
    public CompletableFuture<Void> connect(Serializable userid, InetSocketAddress sncpAddr) {
        CompletableFuture<Void> future = sncpNodeAddresses.appendSetItemAsync(SOURCE_SNCP_USERID_PREFIX + userid, sncpAddr);
        future = future.thenAccept((a) -> sncpNodeAddresses.incr(SOURCE_SNCP_USERCOUNT_KEY));
        future = future.thenAccept((a) -> sncpNodeAddresses.appendSetItemAsync(SOURCE_SNCP_ADDRS_KEY, sncpAddr));
        if (logger.isLoggable(Level.FINEST)) logger.finest(WebSocketNodeService.class.getSimpleName() + ".event: " + userid + " connect from " + sncpAddr);
        return future;
    }

    /**
     * 当用户从一个节点断掉了所有的连接，需要从CacheSource中删除
     *
     * @param userid   Serializable
     * @param sncpAddr InetSocketAddress
     *
     * @return 无返回值
     */
    @Override
    public CompletableFuture<Void> disconnect(Serializable userid, InetSocketAddress sncpAddr) {
        CompletableFuture<Void> future = sncpNodeAddresses.removeSetItemAsync(SOURCE_SNCP_USERID_PREFIX + userid, sncpAddr);
        future = future.thenAccept((a) -> sncpNodeAddresses.decr(SOURCE_SNCP_USERCOUNT_KEY));
        if (logger.isLoggable(Level.FINEST)) logger.finest(WebSocketNodeService.class.getSimpleName() + ".event: " + userid + " disconnect from " + sncpAddr);
        return future;
    }

    /**
     * 更改用户ID，需要更新到CacheSource
     *
     * @param olduserid Serializable
     * @param newuserid Serializable
     * @param sncpAddr  InetSocketAddress
     *
     * @return 无返回值
     */
    @Override
    public CompletableFuture<Void> changeUserid(Serializable olduserid, Serializable newuserid, InetSocketAddress sncpAddr) {
        CompletableFuture<Void> future = sncpNodeAddresses.appendSetItemAsync(SOURCE_SNCP_USERID_PREFIX + newuserid, sncpAddr);
        future = future.thenAccept((a) -> sncpNodeAddresses.removeSetItemAsync(SOURCE_SNCP_USERID_PREFIX + olduserid, sncpAddr));
        if (logger.isLoggable(Level.FINEST)) logger.finest(WebSocketNodeService.class.getSimpleName() + ".event: " + olduserid + " changeUserid to " + newuserid + " from " + sncpAddr);
        return future;
    }

    /**
     * 强制关闭用户的WebSocket
     *
     * @param userid   Serializable
     * @param sncpAddr InetSocketAddress
     *
     * @return 无返回值
     */
    @Override
    public CompletableFuture<Integer> forceCloseWebSocket(Serializable userid, InetSocketAddress sncpAddr) {
        //不能从sncpNodeAddresses中移除，因为engine.forceCloseWebSocket 会调用到disconnect
        if (logger.isLoggable(Level.FINEST)) logger.finest(WebSocketNodeService.class.getSimpleName() + ".event: " + userid + " forceCloseWebSocket from " + sncpAddr);
        if (localEngine == null) return CompletableFuture.completedFuture(0);
        return CompletableFuture.completedFuture(localEngine.forceCloseLocalWebSocket(userid));
    }
}
