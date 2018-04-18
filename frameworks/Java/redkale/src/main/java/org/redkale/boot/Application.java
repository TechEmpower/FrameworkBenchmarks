/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.boot;

import org.redkale.util.RedkaleClassLoader;
import org.redkale.net.TransportGroupInfo;
import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.channels.*;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.util.logging.*;
import javax.annotation.Resource;
import javax.net.ssl.SSLContext;
import javax.xml.parsers.*;
import org.redkale.boot.ClassFilter.FilterEntry;
import org.redkale.convert.Convert;
import org.redkale.convert.bson.BsonFactory;
import org.redkale.convert.json.JsonFactory;
import org.redkale.net.*;
import org.redkale.net.http.MimeType;
import org.redkale.net.sncp.*;
import org.redkale.service.Service;
import org.redkale.source.*;
import org.redkale.util.AnyValue.DefaultAnyValue;
import org.redkale.util.*;
import org.redkale.watch.*;
import org.w3c.dom.*;

/**
 *
 * 进程启动类，全局对象。  <br>
 * <pre>
 * 程序启动执行步骤:
 *     1、读取application.xml
 *     2、进行classpath扫描动态加载Service、WebSocket与Servlet
 *     3、优先加载所有SNCP协议的服务，再加载其他协议服务， 最后加载WATCH协议的服务
 *     4、最后进行Service、Servlet与其他资源之间的依赖注入
 * </pre>
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public final class Application {

    /**
     * 当前进程启动的时间， 类型： long
     */
    public static final String RESNAME_APP_TIME = "APP_TIME";

    /**
     * 当前进程的根目录， 类型：String、File、Path
     */
    public static final String RESNAME_APP_HOME = "APP_HOME";

    /**
     * application.xml 文件中resources节点的内容， 类型： AnyValue
     */
    public static final String RESNAME_APP_GRES = "APP_GRES";

    /**
     * 当前进程节点的name， 类型：String
     */
    public static final String RESNAME_APP_NODE = "APP_NODE";

    /**
     * 当前进程节点的IP地址， 类型：InetAddress、String
     */
    public static final String RESNAME_APP_ADDR = "APP_ADDR";

    /**
     * 当前Service所属的SNCP Server的地址 类型: SocketAddress、InetSocketAddress、String <br>
     */
    public static final String RESNAME_SNCP_ADDR = "SNCP_ADDR";

    /**
     * 当前Service所属的SNCP Server所属的组 类型: String<br>
     */
    public static final String RESNAME_SNCP_GROUP = "SNCP_GROUP";

    /**
     * "SERVER_ROOT" 当前Server的ROOT目录类型：String、File、Path
     */
    public static final String RESNAME_SERVER_ROOT = Server.RESNAME_SERVER_ROOT;

    /**
     * 当前Server的线程池
     */
    public static final String RESNAME_SERVER_EXECUTOR = Server.RESNAME_SERVER_EXECUTOR;

    //本地IP地址
    final InetAddress localAddress;

    //CacheSource 资源
    final List<CacheSource> cacheSources = new CopyOnWriteArrayList<>();

    //DataSource 资源
    final List<DataSource> dataSources = new CopyOnWriteArrayList<>();

    //NodeServer 资源
    final List<NodeServer> servers = new CopyOnWriteArrayList<>();

    //SNCP传输端的TransportFactory, 注意： 只给SNCP使用
    final TransportFactory sncpTransportFactory;

    //全局根ResourceFactory
    final ResourceFactory resourceFactory = ResourceFactory.root();

    //服务配置项
    final AnyValue config;

    //临时计数器
    CountDownLatch servicecdl;  //会出现两次赋值

    //是否启动了WATCH协议服务
    boolean watching;

    //--------------------------------------------------------------------------------------------    
    //是否用于main方法运行
    private final boolean singletonrun;

    //根WatchFactory
    //private final WatchFactory watchFactory = WatchFactory.root();
    //进程根目录
    private final File home;

    //日志
    private final Logger logger;

    //监听事件
    private final List<ApplicationListener> listeners = new CopyOnWriteArrayList<>();

    //服务启动时间
    private final long startTime = System.currentTimeMillis();

    //Server启动的计数器，用于确保所有Server都启动完后再进行下一步处理
    private final CountDownLatch serversLatch;

    //根ClassLoader
    private final RedkaleClassLoader classLoader;

    //Server根ClassLoader
    private final RedkaleClassLoader serverClassLoader;

    private Application(final AnyValue config) {
        this(false, config);
    }

    private Application(final boolean singletonrun, final AnyValue config) {
        this.singletonrun = singletonrun;
        this.config = config;

        final File root = new File(System.getProperty(RESNAME_APP_HOME));
        this.resourceFactory.register(RESNAME_APP_TIME, long.class, this.startTime);
        this.resourceFactory.register(RESNAME_APP_HOME, Path.class, root.toPath());
        this.resourceFactory.register(RESNAME_APP_HOME, File.class, root);
        try {
            this.resourceFactory.register(RESNAME_APP_HOME, root.getCanonicalPath());
            this.home = root.getCanonicalFile();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        String localaddr = config.getValue("address", "").trim();
        this.localAddress = localaddr.isEmpty() ? Utility.localInetAddress() : new InetSocketAddress(localaddr, config.getIntValue("port")).getAddress();
        this.resourceFactory.register(RESNAME_APP_ADDR, this.localAddress.getHostAddress());
        this.resourceFactory.register(RESNAME_APP_ADDR, InetAddress.class, this.localAddress);
        {
            String node = config.getValue("node", "").trim();
            if (node.isEmpty()) {
                StringBuilder sb = new StringBuilder();
                byte[] bs = this.localAddress.getAddress();
                int v1 = bs[bs.length - 2] & 0xff;
                int v2 = bs[bs.length - 1] & 0xff;
                if (v1 <= 0xf) sb.append('0');
                sb.append(Integer.toHexString(v1));
                if (v2 <= 0xf) sb.append('0');
                sb.append(Integer.toHexString(v2));
                node = sb.toString();
            }
            this.resourceFactory.register(RESNAME_APP_NODE, node);
            System.setProperty(RESNAME_APP_NODE, node);
        }
        //以下是初始化日志配置
        final File logconf = new File(root, "conf/logging.properties");
        if (logconf.isFile() && logconf.canRead()) {
            try {
                final String rootpath = root.getCanonicalPath().replace('\\', '/');
                FileInputStream fin = new FileInputStream(logconf);
                Properties properties = new Properties();
                properties.load(fin);
                fin.close();
                properties.entrySet().stream().forEach(x -> {
                    x.setValue(x.getValue().toString().replace("${APP_HOME}", rootpath));
                });

                if (properties.getProperty("java.util.logging.FileHandler.formatter") == null) {
                    properties.setProperty("java.util.logging.FileHandler.formatter", LogFileHandler.LoggingFormater.class.getName());
                }
                if (properties.getProperty("java.util.logging.ConsoleHandler.formatter") == null) {
                    properties.setProperty("java.util.logging.ConsoleHandler.formatter", LogFileHandler.LoggingFormater.class.getName());
                }
                String fileHandlerPattern = properties.getProperty("java.util.logging.FileHandler.pattern");
                if (fileHandlerPattern != null && fileHandlerPattern.contains("%d")) {
                    final String fileHandlerClass = LogFileHandler.class.getName();
                    Properties prop = new Properties();
                    final String handlers = properties.getProperty("handlers");
                    if (handlers != null && handlers.contains("java.util.logging.FileHandler")) {
                        //singletonrun模式下不输出文件日志
                        prop.setProperty("handlers", handlers.replace("java.util.logging.FileHandler", singletonrun ? "" : fileHandlerClass));
                    }
                    if (!prop.isEmpty()) {
                        String prefix = fileHandlerClass + ".";
                        properties.entrySet().stream().forEach(x -> {
                            if (x.getKey().toString().startsWith("java.util.logging.FileHandler.")) {
                                prop.put(x.getKey().toString().replace("java.util.logging.FileHandler.", prefix), x.getValue());
                            }
                        });
                        prop.entrySet().stream().forEach(x -> {
                            properties.put(x.getKey(), x.getValue());
                        });
                    }
                    properties.put(SncpClient.class.getSimpleName() + ".handlers", LogFileHandler.SncpLogFileHandler.class.getName());
                }
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                final PrintStream ps = new PrintStream(out);
                properties.forEach((x, y) -> ps.println(x + "=" + y));
                LogManager.getLogManager().readConfiguration(new ByteArrayInputStream(out.toByteArray()));
            } catch (Exception e) {
                Logger.getLogger(this.getClass().getSimpleName()).log(Level.WARNING, "init logger configuration error", e);
            }
        }
        this.logger = Logger.getLogger(this.getClass().getSimpleName());
        this.serversLatch = new CountDownLatch(config.getAnyValues("server").length + 1);
        this.classLoader = new RedkaleClassLoader(Thread.currentThread().getContextClassLoader());
        logger.log(Level.INFO, "------------------------- Redkale " + Redkale.getDotedVersion() + " -------------------------");
        //------------------配置 <transport> 节点 ------------------
        ObjectPool<ByteBuffer> transportPool = null;
        ExecutorService transportExec = null;
        AsynchronousChannelGroup transportGroup = null;
        final AnyValue resources = config.getAnyValue("resources");
        TransportStrategy strategy = null;
        int bufferCapacity = 32 * 1024;
        int bufferPoolSize = Runtime.getRuntime().availableProcessors() * 8;
        int readTimeoutSeconds = TransportFactory.DEFAULT_READTIMEOUTSECONDS;
        int writeTimeoutSeconds = TransportFactory.DEFAULT_WRITETIMEOUTSECONDS;
        AtomicLong createBufferCounter = new AtomicLong();
        AtomicLong cycleBufferCounter = new AtomicLong();
        if (resources != null) {
            AnyValue transportConf = resources.getAnyValue("transport");
            int groupsize = resources.getAnyValues("group").length;
            if (groupsize > 0 && transportConf == null) transportConf = new DefaultAnyValue();
            if (transportConf != null) {
                //--------------transportBufferPool-----------
                bufferCapacity = Math.max(parseLenth(transportConf.getValue("bufferCapacity"), bufferCapacity), 8 * 1024);
                readTimeoutSeconds = transportConf.getIntValue("readTimeoutSeconds", readTimeoutSeconds);
                writeTimeoutSeconds = transportConf.getIntValue("writeTimeoutSeconds", writeTimeoutSeconds);
                final int threads = parseLenth(transportConf.getValue("threads"), groupsize * Runtime.getRuntime().availableProcessors() * 2);
                bufferPoolSize = parseLenth(transportConf.getValue("bufferPoolSize"), threads * 4);
                final int capacity = bufferCapacity;
                transportPool = new ObjectPool<>(createBufferCounter, cycleBufferCounter, bufferPoolSize,
                    (Object... params) -> ByteBuffer.allocateDirect(capacity), null, (e) -> {
                        if (e == null || e.isReadOnly() || e.capacity() != capacity) return false;
                        e.clear();
                        return true;
                    });
                //-----------transportChannelGroup--------------
                try {
                    final String strategyClass = transportConf.getValue("strategy");
                    if (strategyClass != null && !strategyClass.isEmpty()) {
                        strategy = (TransportStrategy) classLoader.loadClass(strategyClass).getDeclaredConstructor().newInstance();
                    }
                    final AtomicInteger counter = new AtomicInteger();
                    transportExec = Executors.newFixedThreadPool(threads, (Runnable r) -> {
                        Thread t = new Thread(r);
                        t.setDaemon(true);
                        t.setName("Transport-Thread-" + counter.incrementAndGet());
                        return t;
                    });
                    transportGroup = AsynchronousChannelGroup.withCachedThreadPool(transportExec, 1);
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
                logger.log(Level.INFO, Transport.class.getSimpleName() + " configure bufferCapacity = " + bufferCapacity / 1024 + "K; bufferPoolSize = " + bufferPoolSize + "; threads = " + threads + ";");
            }
        }
        if (transportGroup == null) {
            final AtomicInteger counter = new AtomicInteger();
            transportExec = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() * 8, (Runnable r) -> {
                Thread t = new Thread(r);
                t.setDaemon(true);
                t.setName("Transport-Thread-" + counter.incrementAndGet());
                return t;
            });
            try {
                transportGroup = AsynchronousChannelGroup.withCachedThreadPool(transportExec, 1);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        if (transportPool == null) {
            final int capacity = bufferCapacity;
            transportPool = new ObjectPool<>(createBufferCounter, cycleBufferCounter, bufferPoolSize,
                (Object... params) -> ByteBuffer.allocateDirect(capacity), null, (e) -> {
                    if (e == null || e.isReadOnly() || e.capacity() != capacity) return false;
                    e.clear();
                    return true;
                });
        }
        this.sncpTransportFactory = TransportFactory.create(transportExec, transportPool, transportGroup, (SSLContext) null, readTimeoutSeconds, writeTimeoutSeconds, strategy);
        DefaultAnyValue tarnsportConf = DefaultAnyValue.create(TransportFactory.NAME_POOLMAXCONNS, System.getProperty("net.transport.poolmaxconns", "100"))
            .addValue(TransportFactory.NAME_PINGINTERVAL, System.getProperty("net.transport.pinginterval", "30"))
            .addValue(TransportFactory.NAME_CHECKINTERVAL, System.getProperty("net.transport.checkinterval", "30"));
        this.sncpTransportFactory.init(tarnsportConf, Sncp.PING_BUFFER, Sncp.PONG_BUFFER.remaining());
        Thread.currentThread().setContextClassLoader(this.classLoader);
        this.serverClassLoader = new RedkaleClassLoader(this.classLoader);
    }

    public ResourceFactory getResourceFactory() {
        return resourceFactory;
    }

    public TransportFactory getSncpTransportFactory() {
        return sncpTransportFactory;
    }

    public RedkaleClassLoader getClassLoader() {
        return classLoader;
    }

    public RedkaleClassLoader getServerClassLoader() {
        return serverClassLoader;
    }

    public List<NodeServer> getNodeServers() {
        return new ArrayList<>(servers);
    }

    public File getHome() {
        return home;
    }

    public long getStartTime() {
        return startTime;
    }

    public AnyValue getAppConfig() {
        return config;
    }

    public void init() throws Exception {
        System.setProperty("java.util.concurrent.ForkJoinPool.common.parallelism", "" + Runtime.getRuntime().availableProcessors() * 4);
        System.setProperty("net.transport.poolmaxconns", "100");
        System.setProperty("net.transport.pinginterval", "30");
        System.setProperty("net.transport.checkinterval", "30");
        System.setProperty("convert.bson.tiny", "true");
        System.setProperty("convert.json.tiny", "true");
        System.setProperty("convert.bson.pool.size", "128");
        System.setProperty("convert.json.pool.size", "128");
        System.setProperty("convert.bson.writer.buffer.defsize", "4096");
        System.setProperty("convert.json.writer.buffer.defsize", "4096");

        File persist = new File(this.home, "conf/persistence.xml");
        final String homepath = this.home.getCanonicalPath();
        if (persist.isFile()) System.setProperty(DataSources.DATASOURCE_CONFPATH, persist.getCanonicalPath());
        logger.log(Level.INFO, "APP_JAVA = " + System.getProperty("java.version") + "\r\n" + RESNAME_APP_ADDR + " = " + this.localAddress.getHostAddress() + "\r\n" + RESNAME_APP_HOME + " = " + homepath);
        String lib = config.getValue("lib", "${APP_HOME}/libs/*").trim().replace("${APP_HOME}", homepath);
        lib = lib.isEmpty() ? (homepath + "/conf") : (lib + ";" + homepath + "/conf");
        Server.loadLib(classLoader, logger, lib);

        //------------------------------------------------------------------------
        final AnyValue resources = config.getAnyValue("resources");
        if (resources != null) {
            resourceFactory.register(RESNAME_APP_GRES, AnyValue.class, resources);
            final AnyValue properties = resources.getAnyValue("properties");
            if (properties != null) {
                String dfloads = properties.getValue("load");
                if (dfloads != null) {
                    for (String dfload : dfloads.split(";")) {
                        if (dfload.trim().isEmpty()) continue;
                        final File df = (dfload.indexOf('/') < 0) ? new File(home, "conf/" + dfload) : new File(dfload);
                        if (df.isFile()) {
                            Properties ps = new Properties();
                            InputStream in = new FileInputStream(df);
                            ps.load(in);
                            in.close();
                            ps.forEach((x, y) -> resourceFactory.register("property." + x, y));
                        }
                    }
                }
                for (AnyValue prop : properties.getAnyValues("property")) {
                    String name = prop.getValue("name");
                    String value = prop.getValue("value");
                    if (name == null || value == null) continue;
                    if (name.startsWith("system.property.")) {
                        System.setProperty(name.substring("system.property.".length()), value);
                    } else if (name.startsWith("mimetype.property.")) {
                        MimeType.add(name.substring("mimetype.property.".length()), value);
                    } else if (name.startsWith("property.")) {
                        resourceFactory.register(name, value);
                    } else {
                        resourceFactory.register("property." + name, value);
                    }
                }
            }
        }
        this.resourceFactory.register(BsonFactory.root());
        this.resourceFactory.register(JsonFactory.root());
        this.resourceFactory.register(BsonFactory.root().getConvert());
        this.resourceFactory.register(JsonFactory.root().getConvert());
        this.resourceFactory.register("bsonconvert", Convert.class, BsonFactory.root().getConvert());
        this.resourceFactory.register("jsonconvert", Convert.class, JsonFactory.root().getConvert());
        //只有WatchService才能加载Application、WatchFactory
        final Application application = this;
        this.resourceFactory.register(new ResourceFactory.ResourceLoader() {

            @Override
            public void load(ResourceFactory rf, final Object src, String resourceName, Field field, final Object attachment) {
                try {
                    Resource res = field.getAnnotation(Resource.class);
                    if (res == null) return;
                    if (Sncp.isRemote((Service) src)) return; //远程模式不得注入 
                    Class type = field.getType();
                    if (type == Application.class) {
                        field.set(src, application);
                    } else if (type == ResourceFactory.class) {
                        field.set(src, res.name().equalsIgnoreCase("server") ? rf : (res.name().isEmpty() ? application.resourceFactory : null));
                    } else if (type == TransportFactory.class) {
                        field.set(src, application.sncpTransportFactory);
                    } else if (type == NodeSncpServer.class) {
                        NodeServer server = null;
                        for (NodeServer ns : application.getNodeServers()) {
                            if (ns.getClass() == NodeSncpServer.class) continue;
                            if (res.name().equals(ns.server.getName())) {
                                server = ns;
                                break;
                            }
                        }
                        field.set(src, server);
                    } else if (type == NodeHttpServer.class) {
                        NodeServer server = null;
                        for (NodeServer ns : application.getNodeServers()) {
                            if (ns.getClass() == NodeHttpServer.class) continue;
                            if (res.name().equals(ns.server.getName())) {
                                server = ns;
                                break;
                            }
                        }
                        field.set(src, server);
                    } else if (type == NodeWatchServer.class) {
                        NodeServer server = null;
                        for (NodeServer ns : application.getNodeServers()) {
                            if (ns.getClass() == NodeWatchServer.class) continue;
                            if (res.name().equals(ns.server.getName())) {
                                server = ns;
                                break;
                            }
                        }
                        field.set(src, server);
                    }
//                    if (type == WatchFactory.class) {
//                        field.set(src, application.watchFactory);
//                    }
                } catch (Exception e) {
                    logger.log(Level.SEVERE, "Resource inject error", e);
                }
            }

            @Override
            public boolean autoNone() {
                return false;
            }

        }, Application.class, ResourceFactory.class, TransportFactory.class, NodeSncpServer.class, NodeHttpServer.class, NodeWatchServer.class);
        //--------------------------------------------------------------------------
        initResources();
    }

    private void initResources() throws Exception {
        //-------------------------------------------------------------------------
        final AnyValue resources = config.getAnyValue("resources");
        if (resources != null) {
            //------------------------------------------------------------------------
            for (AnyValue conf : resources.getAnyValues("group")) {
                final String group = conf.getValue("name", "");
                final String protocol = conf.getValue("protocol", Transport.DEFAULT_PROTOCOL).toUpperCase();
                if (!"TCP".equalsIgnoreCase(protocol) && !"UDP".equalsIgnoreCase(protocol)) {
                    throw new RuntimeException("Not supported Transport Protocol " + conf.getValue("protocol"));
                }
                TransportGroupInfo ginfo = new TransportGroupInfo(group, protocol, conf.getValue("subprotocol", ""), new LinkedHashSet<>());
                for (AnyValue node : conf.getAnyValues("node")) {
                    final InetSocketAddress addr = new InetSocketAddress(node.getValue("addr"), node.getIntValue("port"));
                    ginfo.putAddress(addr);
                }
                sncpTransportFactory.addGroupInfo(ginfo);
            }
            for (AnyValue conf : resources.getAnyValues("listener")) {
                final String listenClass = conf.getValue("value", "");
                if (listenClass.isEmpty()) continue;
                Class clazz = classLoader.loadClass(listenClass);
                if (!ApplicationListener.class.isAssignableFrom(clazz)) continue;
                @SuppressWarnings("unchecked")
                ApplicationListener listener = (ApplicationListener) clazz.getDeclaredConstructor().newInstance();
                listener.init(config);
                this.listeners.add(listener);
            }
        }
        //------------------------------------------------------------------------
    }

    public void restoreConfig() throws IOException {
        synchronized (this) {
            File confFile = new File(this.home, "conf/application.xml");
            confFile.renameTo(new File(this.home, "conf/application_" + String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS", System.currentTimeMillis()) + ".xml"));
            final PrintStream ps = new PrintStream(new FileOutputStream(confFile));
            ps.append(config.toXML("application"));
            ps.close();
        }
    }

    private void startSelfServer() throws Exception {
        final Application application = this;
        new Thread() {
            {
                setName("Application-Control-Thread");
            }

            @Override
            public void run() {
                try {
                    final DatagramChannel channel = DatagramChannel.open();
                    channel.configureBlocking(true);
                    channel.socket().setSoTimeout(3000);
                    channel.bind(new InetSocketAddress("127.0.0.1", config.getIntValue("port")));
                    boolean loop = true;
                    ByteBuffer buffer = ByteBuffer.allocateDirect(1024);
                    while (loop) {
                        buffer.clear();
                        SocketAddress address = channel.receive(buffer);
                        buffer.flip();
                        byte[] bytes = new byte[buffer.remaining()];
                        buffer.get(bytes);
                        if ("SHUTDOWN".equalsIgnoreCase(new String(bytes))) {
                            try {
                                long s = System.currentTimeMillis();
                                logger.info(application.getClass().getSimpleName() + " shutdowning");
                                application.shutdown();
                                buffer.clear();
                                buffer.put("SHUTDOWN OK".getBytes());
                                buffer.flip();
                                channel.send(buffer, address);
                                long e = System.currentTimeMillis() - s;
                                logger.info(application.getClass().getSimpleName() + " shutdown in " + e + " ms");
                                application.serversLatch.countDown();
                                System.exit(0);
                            } catch (Exception ex) {
                                logger.log(Level.INFO, "SHUTDOWN FAIL", ex);
                                buffer.clear();
                                buffer.put("SHUTDOWN FAIL".getBytes());
                                buffer.flip();
                                channel.send(buffer, address);
                            }
                        } else if ("APIDOC".equalsIgnoreCase(new String(bytes))) {
                            try {
                                new ApiDocsService(application).run();
                                buffer.clear();
                                buffer.put("APIDOC OK".getBytes());
                                buffer.flip();
                                channel.send(buffer, address);
                            } catch (Exception ex) {
                                buffer.clear();
                                buffer.put("APIDOC FAIL".getBytes());
                                buffer.flip();
                                channel.send(buffer, address);
                            }
                        }
                    }
                } catch (Exception e) {
                    logger.log(Level.INFO, "Control fail", e);
                    System.exit(1);
                }
            }
        }.start();
    }

    private void sendCommand(String command) throws Exception {
        final DatagramChannel channel = DatagramChannel.open();
        channel.configureBlocking(true);
        channel.connect(new InetSocketAddress("127.0.0.1", config.getIntValue("port")));
        ByteBuffer buffer = ByteBuffer.allocate(128);
        buffer.put(command.getBytes());
        buffer.flip();
        channel.write(buffer);
        buffer.clear();
        channel.configureBlocking(false);
        try {
            channel.read(buffer);
            buffer.flip();
            byte[] bytes = new byte[buffer.remaining()];
            buffer.get(bytes);
            channel.close();
            logger.info(new String(bytes));
            Thread.sleep(500);
        } catch (Exception e) {
            if (e instanceof PortUnreachableException) {
                if ("APIDOC".equalsIgnoreCase(command)) {
                    final Application application = Application.create(true);
                    application.init();
                    application.start();
                    new ApiDocsService(application).run();
                    logger.info("APIDOC OK");
                    return;
                }
            }
            throw e;
        }
    }

    public void start() throws Exception {
        final AnyValue[] entrys = config.getAnyValues("server");
        CountDownLatch timecd = new CountDownLatch(entrys.length);
        final List<AnyValue> sncps = new ArrayList<>();
        final List<AnyValue> others = new ArrayList<>();
        final List<AnyValue> watchs = new ArrayList<>();
        for (final AnyValue entry : entrys) {
            if (entry.getValue("protocol", "").toUpperCase().startsWith("SNCP")) {
                sncps.add(entry);
            } else if (entry.getValue("protocol", "").toUpperCase().startsWith("WATCH")) {
                watchs.add(entry);
            } else {
                others.add(entry);
            }
        }
        if (watchs.size() > 1) throw new RuntimeException("Found one more WATCH Server");
        this.watching = !watchs.isEmpty();

        runServers(timecd, sncps);  //必须确保SNCP服务都启动后再启动其他服务
        runServers(timecd, others);
        runServers(timecd, watchs); //必须在所有服务都启动后再启动WATCH服务
        timecd.await();
        //if (!singletonrun) signalHandle();
        if (!singletonrun) clearPersistData();
        logger.info(this.getClass().getSimpleName() + " started in " + (System.currentTimeMillis() - startTime) + " ms\r\n");
        if (!singletonrun) this.serversLatch.await();
    }

    private void clearPersistData() {
        File cachedir = new File(home, "cache");
        if (!cachedir.isDirectory()) return;
        for (File file : cachedir.listFiles()) {
            if (file.getName().startsWith("persist-")) file.delete();
        }
    }

//    private void signalHandle() {
//        //http://www.comptechdoc.org/os/linux/programming/linux_pgsignals.html
//        String[] sigs = new String[]{"HUP", "TERM", "INT", "QUIT", "KILL", "TSTP", "USR1", "USR2", "STOP"};
//        List<sun.misc.Signal> list = new ArrayList<>();
//        for (String sig : sigs) {
//            try {
//                list.add(new sun.misc.Signal(sig));
//            } catch (Exception e) {
//            }
//        }
//        sun.misc.SignalHandler handler = new sun.misc.SignalHandler() {
//
//            private volatile boolean runed;
//
//            @Override
//            public void handle(Signal sig) {
//                if (runed) return;
//                runed = true;
//                logger.info(Application.this.getClass().getSimpleName() + " stoped\r\n");
//                System.exit(0);
//            }
//        };
//        for (Signal sig : list) {
//            try {
//                Signal.handle(sig, handler);
//            } catch (Exception e) {
//            }
//        }
//    }
    @SuppressWarnings("unchecked")
    private void runServers(CountDownLatch timecd, final List<AnyValue> serconfs) throws Exception {
        this.servicecdl = new CountDownLatch(serconfs.size());
        CountDownLatch sercdl = new CountDownLatch(serconfs.size());
        final AtomicBoolean inited = new AtomicBoolean(false);
        final Map<String, Class<? extends NodeServer>> nodeClasses = new HashMap<>();
        for (final AnyValue serconf : serconfs) {
            Thread thread = new Thread() {
                {
                    String host = serconf.getValue("host", "0.0.0.0").replace("0.0.0.0", "*");
                    setName(serconf.getValue("protocol", "Server").toUpperCase() + "-" + host + ":" + serconf.getIntValue("port") + "-Thread");
                    this.setDaemon(true);
                }

                @Override
                public void run() {
                    try {
                        //Thread ctd = Thread.currentThread();
                        //ctd.setContextClassLoader(new URLClassLoader(new URL[0], ctd.getContextClassLoader()));
                        final String protocol = serconf.getValue("protocol", "").replaceFirst("\\..+", "").toUpperCase();
                        NodeServer server = null;
                        if ("SNCP".equals(protocol)) {
                            server = NodeSncpServer.createNodeServer(Application.this, serconf);
                        } else if ("WATCH".equalsIgnoreCase(protocol)) {
                            DefaultAnyValue serconf2 = (DefaultAnyValue) serconf;
                            DefaultAnyValue rest = (DefaultAnyValue) serconf2.getAnyValue("rest");
                            if (rest == null) {
                                rest = new DefaultAnyValue();
                                serconf2.addValue("rest", rest);
                            }
                            rest.setValue("base", WatchServlet.class.getName());
                            server = new NodeWatchServer(Application.this, serconf);
                        } else if ("HTTP".equalsIgnoreCase(protocol)) {
                            server = new NodeHttpServer(Application.this, serconf);
                        } else {
                            if (!inited.get()) {
                                synchronized (nodeClasses) {
                                    if (!inited.getAndSet(true)) { //加载自定义的协议，如：SOCKS
                                        ClassFilter profilter = new ClassFilter(classLoader, NodeProtocol.class, NodeServer.class, (Class[]) null);
                                        ClassFilter.Loader.load(home, serconf.getValue("excludelibs", "").split(";"), profilter);
                                        final Set<FilterEntry<NodeServer>> entrys = profilter.getFilterEntrys();
                                        for (FilterEntry<NodeServer> entry : entrys) {
                                            final Class<? extends NodeServer> type = entry.getType();
                                            NodeProtocol pros = type.getAnnotation(NodeProtocol.class);
                                            for (String p : pros.value()) {
                                                p = p.toUpperCase();
                                                if ("SNCP".equals(p) || "HTTP".equals(p)) continue;
                                                final Class<? extends NodeServer> old = nodeClasses.get(p);
                                                if (old != null && old != type) {
                                                    throw new RuntimeException("Protocol(" + p + ") had NodeServer-Class(" + old.getName() + ") but repeat NodeServer-Class(" + type.getName() + ")");
                                                }
                                                nodeClasses.put(p, type);
                                            }
                                        }
                                    }
                                }
                            }
                            Class<? extends NodeServer> nodeClass = nodeClasses.get(protocol);
                            if (nodeClass != null) server = NodeServer.create(nodeClass, Application.this, serconf);
                        }
                        if (server == null) {
                            logger.log(Level.SEVERE, "Not found Server Class for protocol({0})", serconf.getValue("protocol"));
                            System.exit(0);
                        }
                        servers.add(server);
                        server.init(serconf);
                        if (!singletonrun) server.start();
                        timecd.countDown();
                        sercdl.countDown();
                    } catch (Exception ex) {
                        logger.log(Level.WARNING, serconf + " runServers error", ex);
                        Application.this.serversLatch.countDown();
                    }
                }
            };
            thread.start();
        }
        sercdl.await();
    }

    public static <T extends Service> T singleton(Class<T> serviceClass) throws Exception {
        return singleton("", serviceClass);
    }

    public static <T extends Service> T singleton(String name, Class<T> serviceClass) throws Exception {
        if (serviceClass == null) throw new IllegalArgumentException("serviceClass is null");
        final Application application = Application.create(true);
        application.init();
        application.start();
        for (NodeServer server : application.servers) {
            T service = server.resourceFactory.find(name, serviceClass);
            if (service != null) return service;
        }
        if (Modifier.isAbstract(serviceClass.getModifiers())) throw new IllegalArgumentException("abstract class not allowed");
        if (serviceClass.isInterface()) throw new IllegalArgumentException("interface class not allowed");
        throw new IllegalArgumentException(serviceClass.getName() + " maybe have zero not-final public method");
    }

    public static Application create(final boolean singleton) throws IOException {
        final String home = new File(System.getProperty(RESNAME_APP_HOME, "")).getCanonicalPath().replace('\\', '/');
        System.setProperty(RESNAME_APP_HOME, home);
        File appfile = new File(home, "conf/application.xml");
        return new Application(singleton, load(new FileInputStream(appfile)));
    }

    public static void main(String[] args) throws Exception {
        Utility.midnight(); //先初始化一下Utility
        //运行主程序
        final Application application = Application.create(false);
        if (System.getProperty("CMD") != null) {
            application.sendCommand(System.getProperty("CMD"));
            return;
        } else if (System.getProperty("SHUTDOWN") != null) { //兼容旧接口
            application.sendCommand("SHUTDOWN");
            return;
        }
        application.init();
        application.startSelfServer();
        try {
            for (ApplicationListener listener : application.listeners) {
                listener.preStart(application);
            }
            application.start();
        } catch (Exception e) {
            application.logger.log(Level.SEVERE, "Application start error", e);
            System.exit(0);
        }
        System.exit(0);
    }

    NodeSncpServer findNodeSncpServer(final InetSocketAddress sncpAddr) {
        for (NodeServer node : servers) {
            if (node.isSNCP() && sncpAddr.equals(node.getSncpAddress())) {
                return (NodeSncpServer) node;
            }
        }
        return null;
    }

    private void shutdown() throws Exception {
        for (ApplicationListener listener : this.listeners) {
            try {
                listener.preShutdown(this);
            } catch (Exception e) {
                logger.log(Level.WARNING, listener.getClass() + " preShutdown erroneous", e);
            }
        }

        servers.stream().forEach((server) -> {
            try {
                server.shutdown();
            } catch (Exception t) {
                logger.log(Level.WARNING, " shutdown server(" + server.getSocketAddress() + ") error", t);
            } finally {
                serversLatch.countDown();
            }
        });

        for (DataSource source : dataSources) {
            if (source == null) continue;
            try {
                source.getClass().getMethod("close").invoke(source);
            } catch (Exception e) {
                logger.log(Level.FINER, source.getClass() + " close DataSource erroneous", e);
            }
        }
        for (CacheSource source : cacheSources) {
            if (source == null) continue;
            try {
                source.getClass().getMethod("close").invoke(source);
            } catch (Exception e) {
                logger.log(Level.FINER, source.getClass() + " close CacheSource erroneous", e);
            }
        }
        this.sncpTransportFactory.shutdownNow();
    }

    private static int parseLenth(String value, int defValue) {
        if (value == null) return defValue;
        value = value.toUpperCase().replace("B", "");
        if (value.endsWith("G")) return Integer.decode(value.replace("G", "")) * 1024 * 1024 * 1024;
        if (value.endsWith("M")) return Integer.decode(value.replace("M", "")) * 1024 * 1024;
        if (value.endsWith("K")) return Integer.decode(value.replace("K", "")) * 1024;
        return Integer.decode(value);
    }

    private static AnyValue load(final InputStream in0) {
        final DefaultAnyValue any = new DefaultAnyValue();
        try (final InputStream in = in0) {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse(in);
            Element root = doc.getDocumentElement();
            load(any, root);
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
        return any;
    }

    private static void load(final DefaultAnyValue any, final Node root) {
        final String home = System.getProperty(RESNAME_APP_HOME);
        NamedNodeMap nodes = root.getAttributes();
        if (nodes == null) return;
        for (int i = 0; i < nodes.getLength(); i++) {
            Node node = nodes.item(i);
            any.addValue(node.getNodeName(), node.getNodeValue().replace("${APP_HOME}", home));
        }
        NodeList children = root.getChildNodes();
        if (children == null) return;
        for (int i = 0; i < children.getLength(); i++) {
            Node node = children.item(i);
            if (node.getNodeType() != Node.ELEMENT_NODE) continue;
            DefaultAnyValue sub = new DefaultAnyValue();
            load(sub, node);
            any.addValue(node.getNodeName(), sub);
        }

    }
}
