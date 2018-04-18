/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net;

import java.io.*;
import java.net.*;
import java.nio.charset.Charset;
import java.text.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;
import javax.net.ssl.SSLContext;
import org.redkale.util.*;

/**
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <K> 请求ID的数据类型， 例如HTTP协议请求标识为url，请求ID的数据类型就是String
 * @param <C> Context
 * @param <R> Request
 * @param <P> Response
 * @param <S> Servlet
 */
public abstract class Server<K extends Serializable, C extends Context, R extends Request<C>, P extends Response<C, R>, S extends Servlet<C, R, P>> {

    public static final String RESNAME_SERVER_ROOT = "SERVER_ROOT";

    public static final String RESNAME_SERVER_EXECUTOR = "SERVER_EXECUTOR";

    protected final Logger logger = Logger.getLogger(this.getClass().getSimpleName());

    //-------------------------------------------------------------
    //服务的启动时间
    protected final long serverStartTime;

    //服务的名称
    protected String name;

    //应用层协议名
    protected final String protocol;

    //依赖注入工厂类
    protected final ResourceFactory resourceFactory;

    //服务的根Servlet
    protected final PrepareServlet<K, C, R, P, S> prepare;

    //SSL
    protected SSLContext sslContext;

    //服务的上下文对象
    protected C context;

    //服务的配置信息
    protected AnyValue config;

    //服务数据的编解码，null视为UTF-8
    protected Charset charset;

    //服务的监听端口
    protected InetSocketAddress address;

    //连接队列大小
    protected int backlog;

    //传输层协议的服务
    protected ProtocolServer serverChannel;

    //ByteBuffer的容量大小
    protected int bufferCapacity;

    //线程数
    protected int threads;

    //线程池
    protected ThreadPoolExecutor executor;

    //ByteBuffer池大小
    protected int bufferPoolSize;

    //Response池大小
    protected int responsePoolSize;

    //请求包大小的上限，单位:字节
    protected int maxbody;

    //Keep-Alive IO读取的超时秒数，小于1视为不设置
    protected int aliveTimeoutSeconds;
    
    //IO读取的超时秒数，小于1视为不设置
    protected int readTimeoutSeconds;

    //IO写入 的超时秒数，小于1视为不设置
    protected int writeTimeoutSeconds;

    //最大连接数
    protected int maxconns;

    protected Server(long serverStartTime, String protocol, ResourceFactory resourceFactory, PrepareServlet<K, C, R, P, S> servlet) {
        this.serverStartTime = serverStartTime;
        this.protocol = protocol;
        this.resourceFactory = resourceFactory;
        this.prepare = servlet;
    }

    public void init(final AnyValue config) throws Exception {
        Objects.requireNonNull(config);
        this.config = config;
        this.address = new InetSocketAddress(config.getValue("host", "0.0.0.0"), config.getIntValue("port", 80));
        this.charset = Charset.forName(config.getValue("charset", "UTF-8"));
        this.maxconns = config.getIntValue("maxconns", 0);
        this.aliveTimeoutSeconds = config.getIntValue("aliveTimeoutSeconds", 0);
        this.readTimeoutSeconds = config.getIntValue("readTimeoutSeconds", 0);
        this.writeTimeoutSeconds = config.getIntValue("writeTimeoutSeconds", 0);
        this.backlog = parseLenth(config.getValue("backlog"), 16 * 1024);
        this.maxbody = parseLenth(config.getValue("maxbody"), 64 * 1024);
        int bufCapacity = parseLenth(config.getValue("bufferCapacity"), 32 * 1024);
        this.bufferCapacity = bufCapacity < 8 * 1024 ? 8 * 1024 : bufCapacity;
        this.threads = config.getIntValue("threads", Runtime.getRuntime().availableProcessors() * 8);
        this.bufferPoolSize = config.getIntValue("bufferPoolSize", this.threads * 4);
        this.responsePoolSize = config.getIntValue("responsePoolSize", this.threads * 2);
        this.name = config.getValue("name", "Server-" + protocol + "-" + this.address.getPort());
        if (!this.name.matches("^[a-zA-Z][\\w_-]{1,64}$")) throw new RuntimeException("server.name (" + this.name + ") is illegal");
        AnyValue sslConf = config.getAnyValue("ssl");
        if (sslConf != null) {
            String creatorClass = sslConf.getValue("creator", SSLCreator.class.getName());
            SSLCreator creator = null;
            if (SSLCreator.class.getName().equals(creatorClass) || creatorClass.isEmpty()) {
                creator = new SSLCreator() {
                };
            } else {
                ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
                creator = ((SSLCreator) classLoader.loadClass(creatorClass).getDeclaredConstructor().newInstance());
            }
            this.resourceFactory.inject(creator);
            this.sslContext = creator.create(this, sslConf);
        }
        final AtomicInteger counter = new AtomicInteger();
        final Format f = createFormat();
        final String n = name;
        this.executor = (ThreadPoolExecutor) Executors.newFixedThreadPool(threads, (Runnable r) -> {
            Thread t = new WorkThread(executor, r);
            t.setName(n + "-ServletThread-" + f.format(counter.incrementAndGet()));
            return t;
        });
    }

    protected static int parseLenth(String value, int defValue) {
        return (int) parseLenth(value, defValue + 0L);
    }

    protected static long parseLenth(String value, long defValue) {
        if (value == null) return defValue;
        value = value.toUpperCase().replace("B", "");
        if (value.endsWith("G")) return Long.decode(value.replace("G", "")) * 1024 * 1024 * 1024;
        if (value.endsWith("M")) return Long.decode(value.replace("M", "")) * 1024 * 1024;
        if (value.endsWith("K")) return Long.decode(value.replace("K", "")) * 1024;
        return Long.decode(value);
    }

    protected static String formatLenth(long value) {
        if (value < 1) return "" + value;
        if (value % (1024 * 1024 * 1024) == 0) return value / (1024 * 1024 * 1024) + "G";
        if (value % (1024 * 1024) == 0) return value / (1024 * 1024) + "M";
        if (value % 1024 == 0) return value / (1024) + "K";
        return value + "B";
    }

    public void destroy(final AnyValue config) throws Exception {
        this.prepare.destroy(context, config);
    }

    public ResourceFactory getResourceFactory() {
        return resourceFactory;
    }

    public ThreadPoolExecutor getExecutor() {
        return executor;
    }

    public InetSocketAddress getSocketAddress() {
        return address;
    }

    public String getName() {
        return name;
    }

    public String getProtocol() {
        return protocol;
    }

    public Logger getLogger() {
        return this.logger;
    }

    public PrepareServlet<K, C, R, P, S> getPrepareServlet() {
        return this.prepare;
    }

    public C getContext() {
        return this.context;
    }

    public void setThreads(int threads) {
        int oldthreads = this.threads;
        this.context.executor.setCorePoolSize(threads);
        this.threads = threads;
        logger.info("[" + Thread.currentThread().getName() + "] " + this.getClass().getSimpleName() + " change threads from " + oldthreads + " to " + threads);
    }

    @SuppressWarnings("unchecked")
    public void addServlet(S servlet, final Object attachment, AnyValue conf, K... mappings) {
        this.prepare.addServlet(servlet, attachment, conf, mappings);
    }

    public void start() throws IOException {
        this.context = this.createContext();
        this.prepare.init(this.context, config);
        this.serverChannel = ProtocolServer.create(this.protocol, context);
        this.serverChannel.open();
        if (this.serverChannel.supportedOptions().contains(StandardSocketOptions.TCP_NODELAY)) {
            this.serverChannel.setOption(StandardSocketOptions.TCP_NODELAY, true);
        }
        serverChannel.bind(address, backlog);
        serverChannel.setMaxconns(this.maxconns);
        serverChannel.accept();
        final String threadName = "[" + Thread.currentThread().getName() + "] ";
        logger.info(threadName + this.getClass().getSimpleName() + ("TCP".equalsIgnoreCase(protocol) ? "" : ("." + protocol)) + " listen: " + address
            + ", threads: " + threads + ", maxbody: " + formatLenth(context.maxbody) + ", bufferCapacity: " + formatLenth(bufferCapacity) + ", bufferPoolSize: " + bufferPoolSize + ", responsePoolSize: " + responsePoolSize
            + ", started in " + (System.currentTimeMillis() - context.getServerStartTime()) + " ms");
    }

    protected abstract C createContext();

    public void shutdown() throws IOException {
        long s = System.currentTimeMillis();
        logger.info(this.getClass().getSimpleName() + "-" + this.protocol + " shutdowning");
        try {
            this.serverChannel.close();
        } catch (Exception e) {
        }
        logger.info(this.getClass().getSimpleName() + "-" + this.protocol + " shutdow prepare servlet");
        this.prepare.destroy(this.context, config);
        long e = System.currentTimeMillis() - s;
        logger.info(this.getClass().getSimpleName() + " shutdown in " + e + " ms");
    }

    /**
     * 判断是否存在Filter
     *
     * @param <T>         泛型
     * @param filterClass Filter类
     *
     * @return boolean
     */
    public <T extends Filter> boolean containsFilter(Class<T> filterClass) {
        return this.prepare.containsFilter(filterClass);
    }

    /**
     * 判断是否存在Filter
     *
     * @param <T>             泛型
     * @param filterClassName Filter类
     *
     * @return boolean
     */
    public <T extends Filter> boolean containsFilter(String filterClassName) {
        return this.prepare.containsFilter(filterClassName);
    }

    /**
     * 判断是否存在Servlet
     *
     * @param servletClass Servlet类
     *
     * @return boolean
     */
    public boolean containsServlet(Class<? extends S> servletClass) {
        return this.prepare.containsServlet(servletClass);
    }

    /**
     * 判断是否存在Servlet
     *
     * @param servletClassName Servlet类
     *
     * @return boolean
     */
    public boolean containsServlet(String servletClassName) {
        return this.prepare.containsServlet(servletClassName);
    }

    /**
     * 销毁Servlet
     *
     * @param servlet Servlet
     */
    public void destroyServlet(S servlet) {
        servlet.destroy(context, this.prepare.getServletConf(servlet));
    }

    //创建数
    public long getCreateConnectionCount() {
        return serverChannel == null ? -1 : serverChannel.getCreateCount();
    }

    //关闭数
    public long getClosedConnectionCount() {
        return serverChannel == null ? -1 : serverChannel.getClosedCount();
    }

    //在线数
    public long getLivingConnectionCount() {
        return serverChannel == null ? -1 : serverChannel.getLivingCount();
    }

    protected Format createFormat() {
        String sf = "0";
        if (this.threads > 10) sf = "00";
        if (this.threads > 100) sf = "000";
        if (this.threads > 1000) sf = "0000";
        return new DecimalFormat(sf);
    }

    public static URL[] loadLib(final RedkaleClassLoader classLoader, final Logger logger, final String lib) throws Exception {
        if (lib == null || lib.isEmpty()) return new URL[0];
        final Set<URL> set = new HashSet<>();
        for (String s : lib.split(";")) {
            if (s.isEmpty()) continue;
            if (s.endsWith("*")) {
                File root = new File(s.substring(0, s.length() - 1));
                if (root.isDirectory()) {
                    for (File f : root.listFiles()) {
                        set.add(f.toURI().toURL());
                    }
                }
            } else {
                File f = new File(s);
                if (f.canRead()) set.add(f.toURI().toURL());
            }
        }
        if (set.isEmpty()) return new URL[0];
        for (URL url : set) {
            classLoader.addURL(url);
        }
        List<URL> list = new ArrayList<>(set);
        Collections.sort(list, (URL o1, URL o2) -> o1.getFile().compareTo(o2.getFile()));
        return list.toArray(new URL[list.size()]);
    }

}
