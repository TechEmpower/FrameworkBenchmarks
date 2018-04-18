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
import java.util.logging.*;
import java.util.stream.Collectors;
import javax.net.ssl.SSLContext;
import org.redkale.service.Service;
import org.redkale.util.*;

/**
 * System.getProperty("net.transport.pinginterval", "30") 心跳周期，默认30秒
 * System.getProperty("net.transport.checkinterval", "30") 检查不可用地址周期，默认30秒
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class TransportFactory {

    @Comment("默认TCP读取超时秒数")
    public static int DEFAULT_READTIMEOUTSECONDS = 6;

    @Comment("默认TCP写入超时秒数")
    public static int DEFAULT_WRITETIMEOUTSECONDS = 6;

    public static final String NAME_POOLMAXCONNS = "poolmaxconns";

    public static final String NAME_PINGINTERVAL = "pinginterval";

    public static final String NAME_CHECKINTERVAL = "checkinterval";

    protected static final Logger logger = Logger.getLogger(TransportFactory.class.getSimpleName());

    //传输端的线程池
    protected final ExecutorService executor;

    //传输端的ByteBuffer对象池
    protected final ObjectPool<ByteBuffer> bufferPool;

    //传输端的ChannelGroup
    protected final AsynchronousChannelGroup channelGroup;

    //每个地址对应的Group名
    protected final Map<InetSocketAddress, String> groupAddrs = new HashMap<>();

    //协议地址的Group集合
    protected final Map<String, TransportGroupInfo> groupInfos = new HashMap<>();

    protected final List<WeakReference<Service>> services = new CopyOnWriteArrayList<>();

    protected final List<WeakReference<Transport>> transportReferences = new CopyOnWriteArrayList<>();

    //连接池大小
    protected int poolmaxconns = Integer.getInteger("net.transport.poolmaxconns", 100);

    //检查不可用地址周期， 单位：秒
    protected int checkinterval = Integer.getInteger("net.transport.checkinterval", 30);

    //心跳周期， 单位：秒
    protected int pinginterval;

    //TCP读取超时秒数
    protected int readTimeoutSeconds;

    //TCP写入超时秒数
    protected int writeTimeoutSeconds;

    //ping和检查的定时器
    private ScheduledThreadPoolExecutor scheduler;

    protected SSLContext sslContext;

    //ping的内容
    private ByteBuffer pingBuffer;

    //pong的数据长度, 小于0表示不进行判断
    protected int pongLength;

    //负载均衡策略
    protected final TransportStrategy strategy;

    protected TransportFactory(ExecutorService executor, ObjectPool<ByteBuffer> bufferPool, AsynchronousChannelGroup channelGroup,
        SSLContext sslContext, int readTimeoutSeconds, int writeTimeoutSeconds, final TransportStrategy strategy) {
        this.executor = executor;
        this.bufferPool = bufferPool;
        this.channelGroup = channelGroup;
        this.sslContext = sslContext;
        this.readTimeoutSeconds = readTimeoutSeconds;
        this.writeTimeoutSeconds = writeTimeoutSeconds;
        this.strategy = strategy;
    }

    protected TransportFactory(ExecutorService executor, ObjectPool<ByteBuffer> bufferPool, AsynchronousChannelGroup channelGroup,
        SSLContext sslContext, int readTimeoutSeconds, int writeTimeoutSeconds) {
        this(executor, bufferPool, channelGroup, sslContext, readTimeoutSeconds, writeTimeoutSeconds, null);
    }

    public void init(AnyValue conf, ByteBuffer pingBuffer, int pongLength) {
        if (conf != null) {
            this.poolmaxconns = conf.getIntValue(NAME_POOLMAXCONNS, this.poolmaxconns);
            this.pinginterval = conf.getIntValue(NAME_PINGINTERVAL, this.pinginterval);
            this.checkinterval = conf.getIntValue(NAME_CHECKINTERVAL, this.checkinterval);
            if (this.poolmaxconns < 2) this.poolmaxconns = 2;
            if (this.pinginterval < 2) this.pinginterval = 2;
            if (this.checkinterval < 2) this.checkinterval = 2;
        }
        this.scheduler = new ScheduledThreadPoolExecutor(1, (Runnable r) -> {
            final Thread t = new Thread(r, this.getClass().getSimpleName() + "-TransportFactoryTask-Thread");
            t.setDaemon(true);
            return t;
        });
        this.scheduler.scheduleAtFixedRate(() -> {
            checks();
        }, checkinterval, checkinterval, TimeUnit.SECONDS);

        if (this.pinginterval > 0) {
            if (pingBuffer != null) {
                this.pingBuffer = pingBuffer.asReadOnlyBuffer();
                this.pongLength = pongLength;

                scheduler.scheduleAtFixedRate(() -> {
                    pings();
                }, pinginterval, pinginterval, TimeUnit.SECONDS);
            }
        }
    }

    public static TransportFactory create(int threads) {
        return create(threads, threads * 2, 8 * 1024, DEFAULT_READTIMEOUTSECONDS, DEFAULT_WRITETIMEOUTSECONDS);
    }

    public static TransportFactory create(int threads, int bufferPoolSize, int bufferCapacity) {
        return create(threads, bufferPoolSize, bufferCapacity, DEFAULT_READTIMEOUTSECONDS, DEFAULT_WRITETIMEOUTSECONDS);
    }

    public static TransportFactory create(int threads, int bufferPoolSize, int bufferCapacity, int readTimeoutSeconds, int writeTimeoutSeconds) {
        final ObjectPool<ByteBuffer> transportPool = new ObjectPool<>(new AtomicLong(), new AtomicLong(), bufferPoolSize,
            (Object... params) -> ByteBuffer.allocateDirect(bufferCapacity), null, (e) -> {
                if (e == null || e.isReadOnly() || e.capacity() != bufferCapacity) return false;
                e.clear();
                return true;
            });
        final AtomicInteger counter = new AtomicInteger();
        ExecutorService transportExec = Executors.newFixedThreadPool(threads, (Runnable r) -> {
            Thread t = new Thread(r);
            t.setDaemon(true);
            t.setName("Transport-Thread-" + counter.incrementAndGet());
            return t;
        });
        AsynchronousChannelGroup transportGroup = null;
        try {
            transportGroup = AsynchronousChannelGroup.withCachedThreadPool(transportExec, 1);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return create(transportExec, transportPool, transportGroup, readTimeoutSeconds, writeTimeoutSeconds);
    }

    public static TransportFactory create(ExecutorService executor, ObjectPool<ByteBuffer> bufferPool, AsynchronousChannelGroup channelGroup) {
        return new TransportFactory(executor, bufferPool, channelGroup, null, DEFAULT_READTIMEOUTSECONDS, DEFAULT_WRITETIMEOUTSECONDS, null);
    }

    public static TransportFactory create(ExecutorService executor, ObjectPool<ByteBuffer> bufferPool, AsynchronousChannelGroup channelGroup,
        int readTimeoutSeconds, int writeTimeoutSeconds) {
        return new TransportFactory(executor, bufferPool, channelGroup, null, readTimeoutSeconds, writeTimeoutSeconds, null);
    }

    public static TransportFactory create(ExecutorService executor, ObjectPool<ByteBuffer> bufferPool, AsynchronousChannelGroup channelGroup,
        int readTimeoutSeconds, int writeTimeoutSeconds, final TransportStrategy strategy) {
        return new TransportFactory(executor, bufferPool, channelGroup, null, readTimeoutSeconds, writeTimeoutSeconds, strategy);
    }

    public static TransportFactory create(ExecutorService executor, ObjectPool<ByteBuffer> bufferPool, AsynchronousChannelGroup channelGroup, SSLContext sslContext) {
        return new TransportFactory(executor, bufferPool, channelGroup, sslContext, DEFAULT_READTIMEOUTSECONDS, DEFAULT_WRITETIMEOUTSECONDS, null);
    }

    public static TransportFactory create(ExecutorService executor, ObjectPool<ByteBuffer> bufferPool, AsynchronousChannelGroup channelGroup,
        SSLContext sslContext, int readTimeoutSeconds, int writeTimeoutSeconds) {
        return new TransportFactory(executor, bufferPool, channelGroup, sslContext, readTimeoutSeconds, writeTimeoutSeconds, null);
    }

    public static TransportFactory create(ExecutorService executor, ObjectPool<ByteBuffer> bufferPool, AsynchronousChannelGroup channelGroup,
        SSLContext sslContext, int readTimeoutSeconds, int writeTimeoutSeconds, final TransportStrategy strategy) {
        return new TransportFactory(executor, bufferPool, channelGroup, sslContext, readTimeoutSeconds, writeTimeoutSeconds, strategy);
    }

    public Transport createTransportTCP(String name, final InetSocketAddress clientAddress, final Collection<InetSocketAddress> addresses) {
        return new Transport(name, "TCP", "", this, this.bufferPool, this.channelGroup, this.sslContext, clientAddress, addresses, strategy);
    }

    public Transport createTransport(String name, String protocol, final InetSocketAddress clientAddress, final Collection<InetSocketAddress> addresses) {
        return new Transport(name, protocol, "", this, this.bufferPool, this.channelGroup, this.sslContext, clientAddress, addresses, strategy);
    }

    public Transport createTransport(String name, String protocol, String subprotocol,
        final InetSocketAddress clientAddress, final Collection<InetSocketAddress> addresses) {
        return new Transport(name, protocol, subprotocol, this, this.bufferPool, this.channelGroup, this.sslContext, clientAddress, addresses, strategy);
    }

    public String findGroupName(InetSocketAddress addr) {
        if (addr == null) return null;
        return groupAddrs.get(addr);
    }

    public TransportGroupInfo findGroupInfo(String group) {
        if (group == null) return null;
        return groupInfos.get(group);
    }

    public boolean addGroupInfo(String groupName, InetSocketAddress... addrs) {
        addGroupInfo(new TransportGroupInfo(groupName, addrs));
        return true;
    }

    public boolean removeGroupInfo(String groupName, InetSocketAddress addr) {
        if (groupName == null || groupName.isEmpty() || addr == null) return false;
        if (!groupName.equals(groupAddrs.get(addr))) return false;
        TransportGroupInfo group = groupInfos.get(groupName);
        if (group == null) return false;
        group.removeAddress(addr);
        groupAddrs.remove(addr);
        return true;
    }

    public TransportFactory addGroupInfo(String name, Set<InetSocketAddress> addrs) {
        addGroupInfo(new TransportGroupInfo(name, addrs));
        return this;
    }

    public boolean addGroupInfo(TransportGroupInfo info) {
        if (info == null) throw new RuntimeException("TransportGroupInfo can not null");
        if (info.addresses == null) throw new RuntimeException("TransportGroupInfo.addresses can not null");
        if (!checkName(info.name)) throw new RuntimeException("Transport.group.name only 0-9 a-z A-Z _ cannot begin 0-9");
        TransportGroupInfo old = groupInfos.get(info.name);
        if (old != null && !old.protocol.equals(info.protocol)) throw new RuntimeException("Transport.group.name repeat but protocol is different");
        if (old != null && !old.subprotocol.equals(info.subprotocol)) throw new RuntimeException("Transport.group.name repeat but subprotocol is different");
        for (InetSocketAddress addr : info.addresses) {
            if (!groupAddrs.getOrDefault(addr, info.name).equals(info.name)) throw new RuntimeException(addr + " repeat but different group.name");
        }
        if (old == null) {
            groupInfos.put(info.name, info);
        } else {
            old.putAddress(info.addresses);
        }
        for (InetSocketAddress addr : info.addresses) {
            groupAddrs.put(addr, info.name);
        }
        return true;
    }

    public Transport loadSameGroupTransport(InetSocketAddress sncpAddress) {
        return loadTransport(groupAddrs.get(sncpAddress), sncpAddress);
    }

    public Transport[] loadDiffGroupTransports(InetSocketAddress sncpAddress, final Set<String> diffGroups) {
        if (diffGroups == null) return null;
        final String sncpGroup = groupAddrs.get(sncpAddress);
        final List<Transport> transports = new ArrayList<>();
        for (String group : diffGroups) {
            if (sncpGroup == null || !sncpGroup.equals(group)) {
                transports.add(loadTransport(group, sncpAddress));
            }
        }
        return transports.toArray(new Transport[transports.size()]);
    }

    public Transport loadRemoteTransport(InetSocketAddress sncpAddress, final Set<String> groups) {
        if (groups == null) return null;
        Set<InetSocketAddress> addresses = new HashSet<>();
        TransportGroupInfo info = null;
        for (String group : groups) {
            info = groupInfos.get(group);
            if (info == null) continue;
            addresses.addAll(info.addresses);
        }
        if (info == null) info = new TransportGroupInfo("TCP");
        if (sncpAddress != null) addresses.remove(sncpAddress);
        return new Transport(groups.stream().sorted().collect(Collectors.joining(";")), info.protocol, info.subprotocol, this, this.bufferPool, this.channelGroup, this.sslContext, sncpAddress, addresses, this.strategy);
    }

    private Transport loadTransport(final String groupName, InetSocketAddress sncpAddress) {
        if (groupName == null) return null;
        TransportGroupInfo info = groupInfos.get(groupName);
        if (info == null) return null;
        return new Transport(groupName, info.protocol, info.subprotocol, this, this.bufferPool, this.channelGroup, this.sslContext, sncpAddress, info.addresses, this.strategy);
    }

    public ExecutorService getExecutor() {
        return executor;
    }

    public Supplier<ByteBuffer> getBufferSupplier() {
        return bufferPool;
    }

    public List<TransportGroupInfo> getGroupInfos() {
        return new ArrayList<>(this.groupInfos.values());
    }

    public Logger getLogger() {
        return logger;
    }

    public void addSncpService(Service service) {
        if (service == null) return;
        services.add(new WeakReference<>(service));
    }

    public List<Service> getServices() {
        List<Service> rs = new ArrayList<>();
        for (WeakReference<Service> ref : services) {
            Service service = ref.get();
            if (service != null) rs.add(service);
        }
        return rs;
    }

    public void shutdownNow() {
        if (this.scheduler != null) this.scheduler.shutdownNow();
        try {
            this.channelGroup.shutdownNow();
        } catch (Exception e) {
            logger.log(Level.FINER, "close transportChannelGroup erroneous", e);
        }
    }

    private void checks() {
        List<WeakReference> nulllist = new ArrayList<>();
        for (WeakReference<Transport> ref : transportReferences) {
            Transport transport = ref.get();
            if (transport == null) {
                nulllist.add(ref);
                continue;
            }
            Transport.TransportNode[] nodes = transport.getTransportNodes();
            for (final Transport.TransportNode node : nodes) {
                if (node.disabletime < 1) continue; //可用
                try {
                    final AsynchronousSocketChannel channel = AsynchronousSocketChannel.open(transport.group);
                    channel.connect(node.address, node, new CompletionHandler<Void, Transport.TransportNode>() {
                        @Override
                        public void completed(Void result, Transport.TransportNode attachment) {
                            attachment.disabletime = 0;
                        }

                        @Override
                        public void failed(Throwable exc, Transport.TransportNode attachment) {
                            attachment.disabletime = System.currentTimeMillis();
                        }
                    });
                } catch (Exception e) {
                }
            }
        }
        for (WeakReference ref : nulllist) {
            transportReferences.remove(ref);
        }
    }

    private void pings() {
        long timex = System.currentTimeMillis() - (this.pinginterval < 15 ? this.pinginterval : (this.pinginterval - 3)) * 1000;
        for (WeakReference<Transport> ref : transportReferences) {
            Transport transport = ref.get();
            if (transport == null) continue;
            Transport.TransportNode[] nodes = transport.getTransportNodes();
            for (final Transport.TransportNode node : nodes) {
                final BlockingQueue<AsyncConnection> queue = node.conns;
                AsyncConnection conn;
                while ((conn = queue.poll()) != null) {
                    if (conn.getLastWriteTime() > timex && false) { //最近几秒内已经进行过IO操作
                        queue.offer(conn);
                    } else { //超过一定时间的连接需要进行ping处理
                        ByteBuffer sendBuffer = pingBuffer.duplicate();
                        final AsyncConnection localconn = conn;
                        final BlockingQueue<AsyncConnection> localqueue = queue;
                        localconn.write(sendBuffer, sendBuffer, new CompletionHandler<Integer, ByteBuffer>() {
                            @Override
                            public void completed(Integer result, ByteBuffer buffer) {
                                if (buffer.hasRemaining()) {
                                    localconn.write(buffer, buffer, this);
                                    return;
                                }
                                ByteBuffer pongBuffer = bufferPool.get();
                                localconn.read(pongBuffer, pongBuffer, new CompletionHandler<Integer, ByteBuffer>() {
                                    int counter = 0;

                                    @Override
                                    public void completed(Integer result, ByteBuffer attachment) {
                                        if (counter > 3) {
                                            bufferPool.accept(attachment);
                                            localconn.dispose();
                                            return;
                                        }
                                        if (pongLength > 0 && attachment.position() < pongLength) {
                                            counter++;
                                            localconn.read(pongBuffer, pongBuffer, this);
                                            return;
                                        }
                                        bufferPool.accept(attachment);
                                        localqueue.offer(localconn);
                                    }

                                    @Override
                                    public void failed(Throwable exc, ByteBuffer attachment) {
                                        localconn.dispose();
                                    }
                                });
                            }

                            @Override
                            public void failed(Throwable exc, ByteBuffer buffer) {
                                localconn.dispose();
                            }
                        });
                    }
                }
            }
        }
    }

    private static boolean checkName(String name) {  //不能含特殊字符
        if (name.isEmpty()) return false;
        if (name.charAt(0) >= '0' && name.charAt(0) <= '9') return false;
        for (char ch : name.toCharArray()) {
            if (!((ch >= '0' && ch <= '9') || ch == '_' || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))) { //不能含特殊字符
                return false;
            }
        }
        return true;
    }
}
