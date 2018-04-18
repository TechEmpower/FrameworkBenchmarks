/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net;

import java.io.IOException;
import java.net.*;
import java.nio.*;
import java.nio.channels.AsynchronousChannelGroup;
import java.nio.charset.*;
import java.util.concurrent.*;
import java.util.function.*;
import java.util.logging.*;
import javax.net.ssl.SSLContext;
import org.redkale.convert.bson.*;
import org.redkale.convert.json.*;
import org.redkale.util.*;

/**
 * 服务器上下文对象
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class Context {

    //服务启动时间
    protected final long serverStartTime;

    //Server的线程池
    protected final ThreadPoolExecutor executor;

    //SSL
    protected final SSLContext sslContext;

    //ByteBuffer的容量，默认8K
    protected final int bufferCapacity;

    //ByteBuffer对象池
    protected final ObjectPool<ByteBuffer> bufferPool;

    //Response对象池
    protected final ObjectPool<Response> responsePool;

    //服务的根Servlet
    protected final PrepareServlet prepare;

    //服务的监听地址
    private final InetSocketAddress address;

    //字符集
    protected final Charset charset;

    //请求内容的大小上限, 默认64K
    protected final int maxbody;

    //keep alive IO读取的超时时间
    protected final int aliveTimeoutSeconds;

    //IO读取的超时时间
    protected final int readTimeoutSeconds;

    //IO写入的超时时间
    protected final int writeTimeoutSeconds;

    //日志Logger
    protected final Logger logger;

    //BSON操作工厂
    protected final BsonFactory bsonFactory;

    //JSON操作工厂
    protected final JsonFactory jsonFactory;

    //依赖注入工厂类
    protected final ResourceFactory resourceFactory;

    public Context(long serverStartTime, Logger logger, ThreadPoolExecutor executor, SSLContext sslContext,
        int bufferCapacity, ObjectPool<ByteBuffer> bufferPool, ObjectPool<Response> responsePool,
        final int maxbody, Charset charset, InetSocketAddress address, ResourceFactory resourceFactory,
        final PrepareServlet prepare, final int aliveTimeoutSeconds, final int readTimeoutSeconds, final int writeTimeoutSeconds) {
        this.serverStartTime = serverStartTime;
        this.logger = logger;
        this.executor = executor;
        this.sslContext = sslContext;
        this.bufferCapacity = bufferCapacity;
        this.bufferPool = bufferPool;
        this.responsePool = responsePool;
        this.maxbody = maxbody;
        this.charset = StandardCharsets.UTF_8.equals(charset) ? null : charset;
        this.address = address;
        this.prepare = prepare;
        this.resourceFactory = resourceFactory;
        this.aliveTimeoutSeconds = aliveTimeoutSeconds;
        this.readTimeoutSeconds = readTimeoutSeconds;
        this.writeTimeoutSeconds = writeTimeoutSeconds;
        this.jsonFactory = JsonFactory.root();
        this.bsonFactory = BsonFactory.root();
    }

    public ResourceFactory getResourceFactory() {
        return resourceFactory;
    }

    public SSLContext getSSLContext() {
        return sslContext;
    }

    public int getMaxbody() {
        return maxbody;
    }

    public InetSocketAddress getServerAddress() {
        return address;
    }

    public long getServerStartTime() {
        return serverStartTime;
    }

    public Charset getCharset() {
        return charset;
    }

    public Future<?> submitAsync(Runnable r) {
        return executor.submit(r);
    }

    public AsynchronousChannelGroup createAsynchronousChannelGroup() throws IOException {
        return AsynchronousChannelGroup.withThreadPool(executor);
    }

    public void runAsync(Runnable r) {
        executor.execute(r);
    }

    public int getBufferCapacity() {
        return bufferCapacity;
    }

    public Supplier<ByteBuffer> getBufferSupplier() {
        return bufferPool;
    }

    public Consumer<ByteBuffer> getBufferConsumer() {
        return bufferPool;
    }

    public ByteBuffer pollBuffer() {
        return bufferPool.get();
    }

    public void offerBuffer(ByteBuffer buffer) {
        bufferPool.accept(buffer);
    }

    public void offerBuffer(ByteBuffer... buffers) {
        if (buffers == null) return;
        for (ByteBuffer buffer : buffers) {
            bufferPool.accept(buffer);
        }
    }

    public Logger getLogger() {
        return logger;
    }

    public int getAliveTimeoutSeconds() {
        return aliveTimeoutSeconds;
    }

    public int getReadTimeoutSeconds() {
        return readTimeoutSeconds;
    }

    public int getWriteTimeoutSeconds() {
        return writeTimeoutSeconds;
    }

    public JsonConvert getJsonConvert() {
        return jsonFactory.getConvert();
    }

    public BsonConvert getBsonConvert() {
        return bsonFactory.getConvert();
    }
}
