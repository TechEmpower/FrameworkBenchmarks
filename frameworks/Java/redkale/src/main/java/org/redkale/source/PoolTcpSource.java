/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.*;
import java.sql.SQLException;
import java.util.Properties;
import java.util.concurrent.*;
import java.util.logging.Logger;
import org.redkale.net.AsyncConnection;
import org.redkale.util.ObjectPool;

/**
 *
 * @author zhangjx
 */
public abstract class PoolTcpSource extends PoolSource<AsyncConnection> {

    //ByteBuffer池
    protected ObjectPool<ByteBuffer> bufferPool;

    //线程池
    protected ThreadPoolExecutor executor;

    //TCP Channel组
    protected AsynchronousChannelGroup group;

    public PoolTcpSource(String rwtype, Properties prop, Logger logger, ObjectPool<ByteBuffer> bufferPool, ThreadPoolExecutor executor) {
        super(rwtype, prop, logger);
        this.bufferPool = bufferPool;
        this.executor = executor;
        try {
            this.group = AsynchronousChannelGroup.withThreadPool(executor);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public final boolean isAysnc() {
        return true;
    }

    @Override
    public final AsyncConnection poll() {
        return pollAsync().join();
    }

    protected abstract ByteBuffer reqConnectBuffer(AsyncConnection conn);

    protected abstract void respConnectBuffer(final ByteBuffer buffer, CompletableFuture<AsyncConnection> future, AsyncConnection conn);

    @Override
    public CompletableFuture<AsyncConnection> pollAsync() {
        return AsyncConnection.createTCP(group, this.servaddr, this.readTimeoutSeconds, this.writeTimeoutSeconds).thenCompose(conn -> {
            CompletableFuture<AsyncConnection> future = new CompletableFuture();
            final ByteBuffer buffer = reqConnectBuffer(conn);
            conn.write(buffer, null, new CompletionHandler<Integer, Void>() {
                @Override
                public void completed(Integer result, Void attachment1) {
                    if (result < 0) {
                        failed(new SQLException("Write Buffer Error"), attachment1);
                        return;
                    }
                    if (buffer.hasRemaining()) {
                        conn.write(buffer, attachment1, this);
                        return;
                    }
                    buffer.clear();
                    conn.read(buffer, null, new CompletionHandler<Integer, Void>() {
                        @Override
                        public void completed(Integer result, Void attachment2) {
                            if (result < 0) {
                                failed(new SQLException("Read Buffer Error"), attachment2);
                                return;
                            }
                            buffer.flip();
                            respConnectBuffer(buffer, future, conn);
                        }

                        @Override
                        public void failed(Throwable exc, Void attachment2) {
                            bufferPool.accept(buffer);
                            future.completeExceptionally(exc);
                            conn.dispose();
                        }
                    });
                }

                @Override
                public void failed(Throwable exc, Void attachment1) {
                    bufferPool.accept(buffer);
                    future.completeExceptionally(exc);
                    conn.dispose();
                }
            });
            return future;
        });
    }
}
