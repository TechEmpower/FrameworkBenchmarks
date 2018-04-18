/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net;

import java.net.SocketAddress;
import java.util.concurrent.CompletableFuture;

/**
 * 远程请求的负载均衡策略
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public interface TransportStrategy {

    /**
     * 创建AsyncConnection
     *
     * @param addr      服务器地址
     * @param transport Transport
     *
     * @return AsyncConnection
     */
    public CompletableFuture<AsyncConnection> pollConnection(SocketAddress addr, Transport transport);

    /**
     * 回收AsyncConnection，返回false表示使用Transport默认的回收实现， 返回true表示自定义回收实现
     *
     * @param forceClose 是否强制关闭
     * @param conn       AsyncConnection
     *
     * @return boolean
     */
    default boolean offerConnection(final boolean forceClose, AsyncConnection conn) {
        return false;
    }
}
