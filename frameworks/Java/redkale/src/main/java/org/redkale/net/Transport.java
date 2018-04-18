/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net;

import java.io.IOException;
import java.lang.ref.WeakReference;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.channels.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.util.function.Supplier;
import java.util.logging.Level;
import javax.net.ssl.SSLContext;
import org.redkale.convert.*;
import org.redkale.convert.json.JsonConvert;
import org.redkale.util.*;

/**
 * 传输客户端
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public final class Transport {

    public static final String DEFAULT_PROTOCOL = "TCP";

    protected static final boolean supportTcpNoDelay;

    static {
        boolean tcpNoDelay = false;
        try {
            AsynchronousSocketChannel channel = AsynchronousSocketChannel.open();
            tcpNoDelay = channel.supportedOptions().contains(StandardSocketOptions.TCP_NODELAY);
            channel.close();
        } catch (Exception e) {
        }
        supportTcpNoDelay = tcpNoDelay;
    }

    protected final AtomicInteger seq = new AtomicInteger(-1);

    protected final TransportFactory factory;

    protected final String name; //即<group>的name属性

    protected final String subprotocol; //即<group>的subprotocol属性

    protected final boolean tcp;

    protected final String protocol;

    protected final AsynchronousChannelGroup group;

    protected final InetSocketAddress clientAddress;

    //不可能为null
    protected TransportNode[] transportNodes = new TransportNode[0];

    protected final ObjectPool<ByteBuffer> bufferPool;

    protected final SSLContext sslContext;

    //负载均衡策略
    protected final TransportStrategy strategy;

    protected Transport(String name, String subprotocol, TransportFactory factory, final ObjectPool<ByteBuffer> transportBufferPool,
        final AsynchronousChannelGroup transportChannelGroup, final SSLContext sslContext, final InetSocketAddress clientAddress,
        final Collection<InetSocketAddress> addresses, final TransportStrategy strategy) {
        this(name, DEFAULT_PROTOCOL, subprotocol, factory, transportBufferPool, transportChannelGroup, sslContext, clientAddress, addresses, strategy);
    }

    protected Transport(String name, String protocol, String subprotocol,
        final TransportFactory factory, final ObjectPool<ByteBuffer> transportBufferPool,
        final AsynchronousChannelGroup transportChannelGroup, final SSLContext sslContext, final InetSocketAddress clientAddress,
        final Collection<InetSocketAddress> addresses, final TransportStrategy strategy) {
        this.name = name;
        this.subprotocol = subprotocol == null ? "" : subprotocol.trim();
        this.protocol = protocol;
        this.factory = factory;
        factory.transportReferences.add(new WeakReference<>(this));
        this.tcp = "TCP".equalsIgnoreCase(protocol);
        this.group = transportChannelGroup;
        this.sslContext = sslContext;
        this.bufferPool = transportBufferPool;
        this.clientAddress = clientAddress;
        this.strategy = strategy;
        updateRemoteAddresses(addresses);
    }

    public final InetSocketAddress[] updateRemoteAddresses(final Collection<InetSocketAddress> addresses) {
        final TransportNode[] oldNodes = this.transportNodes;
        synchronized (this) {
            List<TransportNode> list = new ArrayList<>();
            if (addresses != null) {
                for (InetSocketAddress addr : addresses) {
                    if (clientAddress != null && clientAddress.equals(addr)) continue;
                    boolean hasold = false;
                    for (TransportNode oldAddr : oldNodes) {
                        if (oldAddr.getAddress().equals(addr)) {
                            list.add(oldAddr);
                            hasold = true;
                            break;
                        }
                    }
                    if (hasold) continue;
                    list.add(new TransportNode(factory.poolmaxconns, addr));
                }
            }
            this.transportNodes = list.toArray(new TransportNode[list.size()]);
        }
        InetSocketAddress[] rs = new InetSocketAddress[oldNodes.length];
        for (int i = 0; i < rs.length; i++) {
            rs[i] = oldNodes[i].getAddress();
        }
        return rs;
    }

    public final boolean addRemoteAddresses(final InetSocketAddress addr) {
        if (addr == null) return false;
        if (clientAddress != null && clientAddress.equals(addr)) return false;
        synchronized (this) {
            if (this.transportNodes.length == 0) {
                this.transportNodes = new TransportNode[]{new TransportNode(factory.poolmaxconns, addr)};
            } else {
                for (TransportNode i : this.transportNodes) {
                    if (addr.equals(i.address)) return false;
                }
                this.transportNodes = Utility.append(transportNodes, new TransportNode(factory.poolmaxconns, addr));
            }
            return true;
        }
    }

    public final boolean removeRemoteAddresses(InetSocketAddress addr) {
        if (addr == null) return false;
        synchronized (this) {
            this.transportNodes = Utility.remove(transportNodes, new TransportNode(factory.poolmaxconns, addr));
        }
        return true;
    }

    public String getName() {
        return name;
    }

    public String getSubprotocol() {
        return subprotocol;
    }

    public void close() {
        TransportNode[] nodes = this.transportNodes;
        if (nodes == null) return;
        for (TransportNode node : nodes) {
            if (node != null) node.dispose();
        }
    }

    public InetSocketAddress getClientAddress() {
        return clientAddress;
    }

    public TransportNode[] getTransportNodes() {
        return transportNodes;
    }

    public TransportNode findTransportNode(SocketAddress addr) {
        for (TransportNode node : this.transportNodes) {
            if (node.address.equals(addr)) return node;
        }
        return null;
    }

    public InetSocketAddress[] getRemoteAddresses() {
        InetSocketAddress[] rs = new InetSocketAddress[transportNodes.length];
        for (int i = 0; i < rs.length; i++) {
            rs[i] = transportNodes[i].getAddress();
        }
        return rs;
    }

    @Override
    public String toString() {
        return Transport.class.getSimpleName() + "{name = " + name + ", protocol = " + protocol + ", clientAddress = " + clientAddress + ", remoteNodes = " + Arrays.toString(transportNodes) + "}";
    }

    public ByteBuffer pollBuffer() {
        return bufferPool.get();
    }

    public Supplier<ByteBuffer> getBufferSupplier() {
        return bufferPool;
    }

    public void offerBuffer(ByteBuffer buffer) {
        bufferPool.accept(buffer);
    }

    public void offerBuffer(ByteBuffer... buffers) {
        for (ByteBuffer buffer : buffers) offerBuffer(buffer);
    }

    public AsynchronousChannelGroup getTransportChannelGroup() {
        return group;
    }

    public boolean isTCP() {
        return tcp;
    }

    public CompletableFuture<AsyncConnection> pollConnection(SocketAddress addr0) {
        if (this.strategy != null) return strategy.pollConnection(addr0, this);
        final TransportNode[] nodes = this.transportNodes;
        if (addr0 == null && nodes.length == 1) addr0 = nodes[0].address;
        final SocketAddress addr = addr0;
        final boolean rand = addr == null; //是否随机取地址
        if (rand && nodes.length < 1) throw new RuntimeException("Transport (" + this.name + ") have no remoteAddress list");
        try {
            if (!tcp) { // UDP
                SocketAddress udpaddr = rand ? nodes[0].address : addr;
                DatagramChannel channel = DatagramChannel.open();
                channel.configureBlocking(true);
                channel.connect(udpaddr);
                return CompletableFuture.completedFuture(AsyncConnection.create(channel, udpaddr, true, factory.readTimeoutSeconds, factory.writeTimeoutSeconds));
            }
            if (!rand) { //指定地址
                TransportNode node = findTransportNode(addr);
                if (node == null) {
                    return AsyncConnection.createTCP(group, sslContext, addr, supportTcpNoDelay, factory.readTimeoutSeconds, factory.writeTimeoutSeconds);
                }
                final BlockingQueue<AsyncConnection> queue = node.conns;
                if (!queue.isEmpty()) {
                    AsyncConnection conn;
                    while ((conn = queue.poll()) != null) {
                        if (conn.isOpen()) return CompletableFuture.completedFuture(conn);
                    }
                }
                return AsyncConnection.createTCP(group, sslContext, addr, supportTcpNoDelay, factory.readTimeoutSeconds, factory.writeTimeoutSeconds);
            }

            //---------------------随机取地址------------------------
            int enablecount = 0;
            final TransportNode[] newnodes = new TransportNode[nodes.length];
            for (final TransportNode node : nodes) {
                if (node.disabletime > 0) continue;
                newnodes[enablecount++] = node;
            }
            final long now = System.currentTimeMillis();
            if (enablecount > 0) { //存在可用的地址
                final TransportNode one = newnodes[Math.abs(seq.incrementAndGet()) % enablecount];
                final BlockingQueue<AsyncConnection> queue = one.conns;
                if (!queue.isEmpty()) {
                    AsyncConnection conn;
                    while ((conn = queue.poll()) != null) {
                        if (conn.isOpen()) return CompletableFuture.completedFuture(conn);
                    }
                }
                CompletableFuture future = new CompletableFuture();
                final AsynchronousSocketChannel channel = AsynchronousSocketChannel.open(group);
                if (supportTcpNoDelay) channel.setOption(StandardSocketOptions.TCP_NODELAY, true);
                channel.connect(one.address, one, new CompletionHandler<Void, TransportNode>() {
                    @Override
                    public void completed(Void result, TransportNode attachment) {
                        attachment.disabletime = 0;
                        AsyncConnection asyncConn = AsyncConnection.create(channel, attachment.address, factory.readTimeoutSeconds, factory.writeTimeoutSeconds);
                        if (future.isDone()) {
                            if (!attachment.conns.offer(asyncConn)) asyncConn.dispose();
                        } else {
                            future.complete(asyncConn);
                        }
                    }

                    @Override
                    public void failed(Throwable exc, TransportNode attachment) {
                        attachment.disabletime = now;
                        try {
                            channel.close();
                        } catch (Exception e) {
                        }
                        try {
                            pollConnection0(nodes, one, now).whenComplete((r, t) -> {
                                if (t != null) {
                                    future.completeExceptionally(t);
                                } else {
                                    future.complete(r);
                                }
                            });

                        } catch (Exception e) {
                            future.completeExceptionally(e);
                        }
                    }
                });
                return future;
            }
            return pollConnection0(nodes, null, now);
        } catch (Exception ex) {
            throw new RuntimeException("transport address = " + addr, ex);
        }
    }

    private CompletableFuture<AsyncConnection> pollConnection0(TransportNode[] nodes, TransportNode exclude, long now) throws IOException {
        //从可用/不可用的地址列表中创建连接
        AtomicInteger count = new AtomicInteger(nodes.length);
        CompletableFuture future = new CompletableFuture();
        for (final TransportNode node : nodes) {
            if (node == exclude) continue;
            if (future.isDone()) return future;
            final AsynchronousSocketChannel channel = AsynchronousSocketChannel.open(group);
            if (supportTcpNoDelay) channel.setOption(StandardSocketOptions.TCP_NODELAY, true);
            channel.connect(node.address, node, new CompletionHandler<Void, TransportNode>() {
                @Override
                public void completed(Void result, TransportNode attachment) {
                    attachment.disabletime = 0;
                    AsyncConnection asyncConn = AsyncConnection.create(channel, attachment.address, factory.readTimeoutSeconds, factory.writeTimeoutSeconds);
                    if (future.isDone()) {
                        if (!attachment.conns.offer(asyncConn)) asyncConn.dispose();
                    } else {
                        future.complete(asyncConn);
                    }
                }

                @Override
                public void failed(Throwable exc, TransportNode attachment) {
                    attachment.disabletime = now;
                    if (count.decrementAndGet() < 1) {
                        future.completeExceptionally(exc);
                    }
                    try {
                        channel.close();
                    } catch (Exception e) {
                    }
                }
            });
        }
        return future;
    }

    public void offerConnection(final boolean forceClose, AsyncConnection conn) {
        if (this.strategy != null && strategy.offerConnection(forceClose, conn)) return;
        if (!forceClose && conn.isTCP()) {
            if (conn.isOpen()) {
                TransportNode node = findTransportNode(conn.getRemoteAddress());
                if (node == null || !node.conns.offer(conn)) conn.dispose();
            }
        } else {
            conn.dispose();
        }
    }

    public <A> void async(SocketAddress addr, final ByteBuffer buffer, A att, final CompletionHandler<Integer, A> handler) {
        pollConnection(addr).whenComplete((conn, ex) -> {
            if (ex != null) {
                factory.getLogger().log(Level.WARNING, Transport.class.getSimpleName() + " async error", ex);
                return;
            }
            conn.write(buffer, buffer, new CompletionHandler<Integer, ByteBuffer>() {

                @Override
                public void completed(Integer result, ByteBuffer attachment) {
                    buffer.clear();
                    conn.read(buffer, buffer, new CompletionHandler<Integer, ByteBuffer>() {

                        @Override
                        public void completed(Integer result, ByteBuffer attachment) {
                            if (handler != null) handler.completed(result, att);
                            offerBuffer(buffer);
                            offerConnection(false, conn);
                        }

                        @Override
                        public void failed(Throwable exc, ByteBuffer attachment) {
                            offerBuffer(buffer);
                            offerConnection(true, conn);
                        }
                    });

                }

                @Override
                public void failed(Throwable exc, ByteBuffer attachment) {
                    offerBuffer(buffer);
                    offerConnection(true, conn);
                }
            });
        });
    }

    public static class TransportNode {

        protected InetSocketAddress address;

        protected volatile long disabletime; //不可用时的时间, 为0表示可用

        protected final BlockingQueue<AsyncConnection> conns;

        protected final ConcurrentHashMap<String, Object> attributes = new ConcurrentHashMap<>();

        public TransportNode(int poolmaxconns, InetSocketAddress address) {
            this.address = address;
            this.disabletime = 0;
            this.conns = new ArrayBlockingQueue<>(poolmaxconns);
        }

        @ConstructorParameters({"poolmaxconns", "address", "disabletime"})
        public TransportNode(int poolmaxconns, InetSocketAddress address, long disabletime) {
            this.address = address;
            this.disabletime = disabletime;
            this.conns = new ArrayBlockingQueue<>(poolmaxconns);
        }

        public int getPoolmaxconns() {
            return this.conns.remainingCapacity() + this.conns.size();
        }

        public <T> T setAttribute(String name, T value) {
            attributes.put(name, value);
            return value;
        }

        @SuppressWarnings("unchecked")
        public <T> T getAttribute(String name) {
            return (T) attributes.get(name);
        }

        @SuppressWarnings("unchecked")
        public <T> T removeAttribute(String name) {
            return (T) attributes.remove(name);
        }

        public TransportNode clearAttributes() {
            attributes.clear();
            return this;
        }

        public ConcurrentHashMap<String, Object> getAttributes() {
            return attributes;
        }

        public void setAttributes(ConcurrentHashMap<String, Object> map) {
            attributes.clear();
            if (map != null) attributes.putAll(map);
        }

        public InetSocketAddress getAddress() {
            return address;
        }

        public long getDisabletime() {
            return disabletime;
        }

        @ConvertDisabled
        public BlockingQueue<AsyncConnection> getConns() {
            return conns;
        }

        public void dispose() {
            AsyncConnection conn;
            while ((conn = conns.poll()) != null) {
                conn.dispose();
            }
        }

        @Override
        public int hashCode() {
            return this.address.hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) return true;
            if (obj == null) return false;
            if (getClass() != obj.getClass()) return false;
            final TransportNode other = (TransportNode) obj;
            return this.address.equals(other.address);
        }

        @Override
        public String toString() {
            return JsonConvert.root().convertTo(this);
        }
    }
}
