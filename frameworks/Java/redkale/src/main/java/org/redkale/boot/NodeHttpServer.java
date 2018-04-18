/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.boot;

import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.net.*;
import java.util.*;
import java.util.logging.Level;
import javax.annotation.*;
import static org.redkale.boot.Application.RESNAME_SNCP_ADDR;
import org.redkale.boot.ClassFilter.FilterEntry;
import org.redkale.net.*;
import org.redkale.net.http.*;
import org.redkale.net.sncp.Sncp;
import org.redkale.service.*;
import org.redkale.util.AnyValue.DefaultAnyValue;
import org.redkale.util.*;
import org.redkale.watch.*;

/**
 * HTTP Server节点的配置Server
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@NodeProtocol({"HTTP"})
public class NodeHttpServer extends NodeServer {

    protected final boolean rest; //是否加载REST服务， 为true加载rest节点信息并将所有可REST化的Service生成RestServlet

    protected final HttpServer httpServer;

    public NodeHttpServer(Application application, AnyValue serconf) {
        super(application, createServer(application, serconf));
        this.httpServer = (HttpServer) server;
        this.rest = serconf == null ? false : serconf.getAnyValue("rest") != null;
    }

    private static Server createServer(Application application, AnyValue serconf) {
        return new HttpServer(application.getStartTime(), application.getResourceFactory().createChild());
    }

    public HttpServer getHttpServer() {
        return httpServer;
    }

    @Override
    public InetSocketAddress getSocketAddress() {
        return httpServer == null ? null : httpServer.getSocketAddress();
    }

    @Override
    @SuppressWarnings("unchecked")
    protected ClassFilter<Service> createServiceClassFilter() {
        return createClassFilter(this.sncpGroup, null, Service.class, new Class[]{org.redkale.watch.WatchService.class}, Annotation.class, "services", "service");
    }

    @Override
    @SuppressWarnings("unchecked")
    protected ClassFilter<Filter> createFilterClassFilter() {
        return createClassFilter(null, null, HttpFilter.class, new Class[]{WatchFilter.class}, null, "filters", "filter");
    }

    @Override
    @SuppressWarnings("unchecked")
    protected ClassFilter<Servlet> createServletClassFilter() {
        return createClassFilter(null, WebServlet.class, HttpServlet.class, new Class[]{WatchServlet.class}, null, "servlets", "servlet");
    }

    @Override
    protected ClassFilter createOtherClassFilter() {
        return createClassFilter(null, RestWebSocket.class, WebSocket.class, null, null, "rest", "websocket");
    }

    @Override
    protected void loadService(ClassFilter<? extends Service> serviceFilter, ClassFilter otherFilter) throws Exception {
        super.loadService(serviceFilter, otherFilter);
        initWebSocketService();
    }

    @Override
    protected void loadFilter(ClassFilter<? extends Filter> filterFilter, ClassFilter otherFilter) throws Exception {
        if (httpServer != null) loadHttpFilter(this.serverConf.getAnyValue("filters"), filterFilter);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected void loadServlet(ClassFilter<? extends Servlet> servletFilter, ClassFilter otherFilter) throws Exception {
        if (httpServer != null) loadHttpServlet(servletFilter, otherFilter);
    }

    private void initWebSocketService() {
        final NodeServer self = this;
        final ResourceFactory regFactory = application.getResourceFactory();
        resourceFactory.register((ResourceFactory rf, final Object src, final String resourceName, Field field, Object attachment) -> { //主要用于单点的服务
            try {
                if (field.getAnnotation(Resource.class) == null) return;
                if (!(src instanceof WebSocketServlet)) return;
                ResourceFactory.ResourceLoader loader = null;
                ResourceFactory sncpResFactory = null;
                for (NodeServer ns : application.servers) {
                    if (!ns.isSNCP()) continue;
                    sncpResFactory = ns.resourceFactory;
                    loader = sncpResFactory.findLoader(WebSocketNode.class, field);
                    if (loader != null) break;
                }
                if (loader != null) loader.load(sncpResFactory, src, resourceName, field, attachment);
                synchronized (regFactory) {
                    Service nodeService = (Service) rf.find(resourceName, WebSocketNode.class);
                    if (sncpResFactory != null && resourceFactory.find(RESNAME_SNCP_ADDR, String.class) == null) {
                        resourceFactory.register(RESNAME_SNCP_ADDR, InetSocketAddress.class, sncpResFactory.find(RESNAME_SNCP_ADDR, InetSocketAddress.class));
                        resourceFactory.register(RESNAME_SNCP_ADDR, SocketAddress.class, sncpResFactory.find(RESNAME_SNCP_ADDR, SocketAddress.class));
                        resourceFactory.register(RESNAME_SNCP_ADDR, String.class, sncpResFactory.find(RESNAME_SNCP_ADDR, String.class));
                    }
                    if (nodeService == null) {
                        nodeService = Sncp.createLocalService(serverClassLoader, resourceName, WebSocketNodeService.class, application.getResourceFactory(), application.getSncpTransportFactory(), (InetSocketAddress) null, (Set<String>) null, (AnyValue) null);
                        regFactory.register(resourceName, WebSocketNode.class, nodeService);
                    }
                    resourceFactory.inject(nodeService, self);
                    logger.fine("[" + Thread.currentThread().getName() + "] Load Service " + nodeService);
                    field.set(src, nodeService);
                }
            } catch (Exception e) {
                logger.log(Level.SEVERE, "WebSocketNode inject error", e);
            }
        }, WebSocketNode.class);
    }

    @SuppressWarnings("unchecked")
    protected void loadHttpFilter(final AnyValue filtersConf, final ClassFilter<? extends Filter> classFilter) throws Exception {
        final StringBuilder sb = logger.isLoggable(Level.INFO) ? new StringBuilder() : null;
        final String threadName = "[" + Thread.currentThread().getName() + "] ";
        List<FilterEntry<? extends Filter>> list = new ArrayList(classFilter.getFilterEntrys());
        for (FilterEntry<? extends Filter> en : list) {
            Class<HttpFilter> clazz = (Class<HttpFilter>) en.getType();
            if (Modifier.isAbstract(clazz.getModifiers())) continue;
            final HttpFilter filter = clazz.getDeclaredConstructor().newInstance();
            resourceFactory.inject(filter, this);
            DefaultAnyValue filterConf = (DefaultAnyValue) en.getProperty();
            this.httpServer.addHttpFilter(filter, filterConf);
            if (sb != null) sb.append(threadName).append(" Load ").append(clazz.getName()).append(LINE_SEPARATOR);
        }
        if (sb != null && sb.length() > 0) logger.log(Level.INFO, sb.toString());
    }

    @SuppressWarnings("unchecked")
    protected void loadHttpServlet(final ClassFilter<? extends Servlet> servletFilter, ClassFilter<? extends WebSocket> webSocketFilter) throws Exception {
        final AnyValue servletsConf = this.serverConf.getAnyValue("servlets");
        final StringBuilder sb = logger.isLoggable(Level.INFO) ? new StringBuilder() : null;
        String prefix0 = servletsConf == null ? "" : servletsConf.getValue("path", "");
        if (!prefix0.isEmpty() && prefix0.charAt(prefix0.length() - 1) == '/') prefix0 = prefix0.substring(0, prefix0.length() - 1);
        if (!prefix0.isEmpty() && prefix0.charAt(0) != '/') prefix0 = '/' + prefix0;
        final String prefix = prefix0;
        final String threadName = "[" + Thread.currentThread().getName() + "] ";
        List<FilterEntry<? extends Servlet>> list = new ArrayList(servletFilter.getFilterEntrys());
        list.sort((FilterEntry<? extends Servlet> o1, FilterEntry<? extends Servlet> o2) -> {  //必须保证WebSocketServlet优先加载， 因为要确保其他的HttpServlet可以注入本地模式的WebSocketNode
            boolean ws1 = WebSocketServlet.class.isAssignableFrom(o1.getType());
            boolean ws2 = WebSocketServlet.class.isAssignableFrom(o2.getType());
            if (ws1 == ws2) {
                Priority p1 = o1.getType().getAnnotation(Priority.class);
                Priority p2 = o2.getType().getAnnotation(Priority.class);
                int v = (p2 == null ? 0 : p2.value()) - (p1 == null ? 0 : p1.value());
                return v == 0 ? o1.getType().getName().compareTo(o2.getType().getName()) : 0;
            }
            return ws1 ? -1 : 1;
        });
        final List<AbstractMap.SimpleEntry<String, String[]>> ss = sb == null ? null : new ArrayList<>();
        for (FilterEntry<? extends Servlet> en : list) {
            Class<HttpServlet> clazz = (Class<HttpServlet>) en.getType();
            if (Modifier.isAbstract(clazz.getModifiers())) continue;
            WebServlet ws = clazz.getAnnotation(WebServlet.class);
            if (ws == null || ws.value().length == 0) continue;
            final HttpServlet servlet = clazz.getDeclaredConstructor().newInstance();
            resourceFactory.inject(servlet, this);
            final String[] mappings = ws.value();
            String pref = ws.repair() ? prefix : "";
            DefaultAnyValue servletConf = (DefaultAnyValue) en.getProperty();
            this.httpServer.addHttpServlet(servlet, pref, servletConf, mappings);
            if (ss != null) {
                for (int i = 0; i < mappings.length; i++) {
                    mappings[i] = pref + mappings[i];
                }
                ss.add(new AbstractMap.SimpleEntry<>(clazz.getName(), mappings));
            }
        }
        int max = 0;
        if (ss != null && sb != null) {
            Collections.sort(ss, (AbstractMap.SimpleEntry<String, String[]> o1, AbstractMap.SimpleEntry<String, String[]> o2) -> o1.getKey().compareTo(o2.getKey()));
            for (AbstractMap.SimpleEntry<String, String[]> as : ss) {
                if (as.getKey().length() > max) max = as.getKey().length();
            }
            for (AbstractMap.SimpleEntry<String, String[]> as : ss) {
                sb.append(threadName).append(" Load ").append(as.getKey());
                for (int i = 0; i < max - as.getKey().length(); i++) {
                    sb.append(' ');
                }
                sb.append("  mapping to  ").append(Arrays.toString(as.getValue())).append(LINE_SEPARATOR);
            }
        }
        if (rest && serverConf != null) {
            final List<Object> restedObjects = new ArrayList<>();
            for (AnyValue restConf : serverConf.getAnyValues("rest")) {
                loadRestServlet(webSocketFilter, restConf, restedObjects, sb);
            }
        }
        if (sb != null && sb.length() > 0) logger.log(Level.INFO, sb.toString().trim());
    }

    @SuppressWarnings("unchecked")
    protected void loadRestServlet(final ClassFilter<? extends WebSocket> webSocketFilter, final AnyValue restConf, final List<Object> restedObjects, final StringBuilder sb) throws Exception {
        if (!rest) return;
        if (restConf == null) return; //不存在REST服务

        String prefix0 = restConf.getValue("path", "");
        if (!prefix0.isEmpty() && prefix0.charAt(prefix0.length() - 1) == '/') prefix0 = prefix0.substring(0, prefix0.length() - 1);
        if (!prefix0.isEmpty() && prefix0.charAt(0) != '/') prefix0 = '/' + prefix0;
        final String prefix = prefix0;

        final String threadName = "[" + Thread.currentThread().getName() + "] ";
        final List<AbstractMap.SimpleEntry<String, String[]>> ss = sb == null ? null : new ArrayList<>();

        final boolean autoload = restConf.getBoolValue("autoload", true);
        {  //加载RestService
            String userTypeStr = restConf.getValue("usertype");
            final Class userType = userTypeStr == null ? null : this.serverClassLoader.loadClass(userTypeStr);

            final Class baseServletType = this.serverClassLoader.loadClass(restConf.getValue("base", HttpServlet.class.getName()));
            final Set<String> includeValues = new HashSet<>();
            final Set<String> excludeValues = new HashSet<>();
            for (AnyValue item : restConf.getAnyValues("service")) {
                if (item.getBoolValue("ignore", false)) {
                    excludeValues.add(item.getValue("value", ""));
                } else {
                    includeValues.add(item.getValue("value", ""));
                }
            }

            final ClassFilter restFilter = ClassFilter.create(null, restConf.getValue("includes", ""), restConf.getValue("excludes", ""), includeValues, excludeValues);
            final boolean finest = logger.isLoggable(Level.FINEST);
            super.interceptorServices.forEach((service) -> {
                final Class stype = Sncp.getServiceType(service);
                final String name = Sncp.getResourceName(service);
                RestService rs = (RestService) stype.getAnnotation(RestService.class);
                if (rs == null || rs.ignore()) return;

                final String stypename = stype.getName();
                if (!autoload && !includeValues.contains(stypename)) return;
                if (!restFilter.accept(stypename)) return;
                if (restedObjects.contains(service)) {
                    logger.log(Level.WARNING, stype.getName() + " repeat create rest servlet, so ignore");
                    return;
                }
                restedObjects.add(service); //避免重复创建Rest对象
                HttpServlet servlet = httpServer.addRestServlet(serverClassLoader, service, userType, baseServletType, prefix);
                if (servlet == null) return; //没有HttpMapping方法的HttpServlet调用Rest.createRestServlet就会返回null 
                String prefix2 = prefix;
                WebServlet ws = servlet.getClass().getAnnotation(WebServlet.class);
                if (ws != null && !ws.repair()) prefix2 = "";
                resourceFactory.inject(servlet, NodeHttpServer.this);
                if (finest) logger.finest(threadName + " Create RestServlet(resource.name='" + name + "') = " + servlet);
                if (ss != null) {
                    String[] mappings = servlet.getClass().getAnnotation(WebServlet.class).value();
                    for (int i = 0; i < mappings.length; i++) {
                        mappings[i] = prefix2 + mappings[i];
                    }
                    ss.add(new AbstractMap.SimpleEntry<>(servlet.getClass().getName(), mappings));
                }
            });
        }
        if (webSocketFilter != null) {  //加载RestWebSocket
            final Set<String> includeValues = new HashSet<>();
            final Set<String> excludeValues = new HashSet<>();
            for (AnyValue item : restConf.getAnyValues("websocket")) {
                if (item.getBoolValue("ignore", false)) {
                    excludeValues.add(item.getValue("value", ""));
                } else {
                    includeValues.add(item.getValue("value", ""));
                }
            }

            final ClassFilter restFilter = ClassFilter.create(null, restConf.getValue("includes", ""), restConf.getValue("excludes", ""), includeValues, excludeValues);
            final boolean finest = logger.isLoggable(Level.FINEST);

            List<FilterEntry<? extends WebSocket>> list = new ArrayList(webSocketFilter.getFilterEntrys());
            for (FilterEntry<? extends WebSocket> en : list) {
                Class<WebSocket> clazz = (Class<WebSocket>) en.getType();
                if (Modifier.isAbstract(clazz.getModifiers())) {
                    logger.log(Level.FINE, clazz.getName() + " cannot abstract on rest websocket, so ignore");
                    continue;
                }
                if (Modifier.isFinal(clazz.getModifiers())) {
                    logger.log(Level.FINE, clazz.getName() + " cannot final on rest websocket, so ignore");
                    continue;
                }
                final Class<? extends WebSocket> stype = en.getType();
                RestWebSocket rs = stype.getAnnotation(RestWebSocket.class);
                if (rs == null || rs.ignore()) return;

                final String stypename = stype.getName();
                if (!autoload && !includeValues.contains(stypename)) return;
                if (!restFilter.accept(stypename)) return;
                if (restedObjects.contains(stype)) {
                    logger.log(Level.WARNING, stype.getName() + " repeat create rest websocket, so ignore");
                    return;
                }
                restedObjects.add(stype); //避免重复创建Rest对象
                HttpServlet servlet = httpServer.addRestWebSocketServlet(serverClassLoader, stype, prefix, en.getProperty());
                if (servlet == null) return; //没有RestOnMessage方法的HttpServlet调用Rest.createRestWebSocketServlet就会返回null 
                String prefix2 = prefix;
                WebServlet ws = servlet.getClass().getAnnotation(WebServlet.class);
                if (ws != null && !ws.repair()) prefix2 = "";
                resourceFactory.inject(servlet, NodeHttpServer.this);
                if (finest) logger.finest(threadName + " " + stype.getName() + " create a RestWebSocketServlet");
                if (ss != null) {
                    String[] mappings = servlet.getClass().getAnnotation(WebServlet.class).value();
                    for (int i = 0; i < mappings.length; i++) {
                        mappings[i] = prefix2 + mappings[i];
                    }
                    ss.add(new AbstractMap.SimpleEntry<>(servlet.getClass().getName(), mappings));
                }
            }
        }
        //输出信息
        if (ss != null && !ss.isEmpty() && sb != null) {
            Collections.sort(ss, (AbstractMap.SimpleEntry<String, String[]> o1, AbstractMap.SimpleEntry<String, String[]> o2) -> o1.getKey().compareTo(o2.getKey()));
            int max = 0;
            for (AbstractMap.SimpleEntry<String, String[]> as : ss) {
                if (as.getKey().length() > max) max = as.getKey().length();
            }
            for (AbstractMap.SimpleEntry<String, String[]> as : ss) {
                sb.append(threadName).append(" Load ").append(as.getKey());
                for (int i = 0; i < max - as.getKey().length(); i++) {
                    sb.append(' ');
                }
                sb.append("  mapping to  ").append(Arrays.toString(as.getValue())).append(LINE_SEPARATOR);
            }
        }
    }
}
