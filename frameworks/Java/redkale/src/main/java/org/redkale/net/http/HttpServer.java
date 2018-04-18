/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import java.lang.reflect.Field;
import java.net.HttpCookie;
import java.nio.ByteBuffer;
import java.util.*;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Level;
import org.redkale.net.*;
import org.redkale.net.sncp.Sncp;
import org.redkale.service.Service;
import org.redkale.util.*;

/**
 * Http服务器
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class HttpServer extends Server<String, HttpContext, HttpRequest, HttpResponse, HttpServlet> {

    public HttpServer() {
        this(System.currentTimeMillis(), ResourceFactory.root());
    }

    public HttpServer(ResourceFactory resourceFactory) {
        this(System.currentTimeMillis(), resourceFactory);
    }

    public HttpServer(long serverStartTime, ResourceFactory resourceFactory) {
        super(serverStartTime, "TCP", resourceFactory, new HttpPrepareServlet());
    }

    @Override
    public void init(AnyValue config) throws Exception {
        super.init(config);
    }

    public List<HttpServlet> getHttpServlets() {
        return this.prepare.getServlets();
    }

    public List<HttpFilter> getHttpFilters() {
        return this.prepare.getFilters();
    }

    /**
     * 获取HttpRender列表
     *
     * @return HttpRender列表
     */
    public List<HttpRender> getHttpRenders() {
        return ((HttpPrepareServlet) this.prepare).renders;
    }

    /**
     * 获取静态资源HttpServlet
     *
     * @return HttpServlet
     */
    public HttpResourceServlet getResourceServlet() {
        return (HttpResourceServlet) ((HttpPrepareServlet) this.prepare).resourceHttpServlet;
    }

    /**
     * 删除HttpServlet
     *
     * @param service Service
     *
     * @return HttpServlet
     */
    public HttpServlet removeHttpServlet(Service service) {
        return ((HttpPrepareServlet) this.prepare).removeHttpServlet(service);
    }

    /**
     * 删除HttpServlet
     *
     * @param <T>                    泛型
     * @param websocketOrServletType Class
     *
     * @return HttpServlet
     */
    public <T extends WebSocket> HttpServlet removeHttpServlet(Class<T> websocketOrServletType) {
        return ((HttpPrepareServlet) this.prepare).removeHttpServlet(websocketOrServletType);
    }

    /**
     * 屏蔽请求URL的正则表达式
     *
     * @param urlreg 正则表达式
     *
     * @return 是否成功
     */
    public boolean addForbidURIReg(final String urlreg) {
        return ((HttpPrepareServlet) this.prepare).addForbidURIReg(urlreg);
    }

    /**
     * 删除屏蔽请求URL的正则表达式
     *
     * @param urlreg 正则表达式
     *
     * @return 是否成功
     */
    public boolean removeForbidURIReg(final String urlreg) {
        return ((HttpPrepareServlet) this.prepare).removeForbidURIReg(urlreg);
    }

    /**
     * 删除HttpFilter
     *
     * @param <T>         泛型
     * @param filterClass HttpFilter类
     *
     * @return HttpFilter
     */
    public <T extends HttpFilter> T removeHttpFilter(Class<T> filterClass) {
        return (T) this.prepare.removeFilter(filterClass);
    }

    /**
     * 添加HttpFilter
     *
     * @param filter HttpFilter
     * @param conf   AnyValue
     *
     * @return HttpServer
     */
    public HttpServer addHttpFilter(HttpFilter filter, AnyValue conf) {
        this.prepare.addFilter(filter, conf);
        return this;
    }

    /**
     * 添加HttpServlet
     *
     * @param prefix   url前缀
     * @param servlet  HttpServlet
     * @param mappings 匹配规则
     *
     * @return HttpServer
     */
    public HttpServer addHttpServlet(String prefix, HttpServlet servlet, String... mappings) {
        this.prepare.addServlet(servlet, prefix, null, mappings);
        return this;
    }

    /**
     * 添加HttpServlet
     *
     * @param servlet  HttpServlet
     * @param mappings 匹配规则
     *
     * @return HttpServer
     */
    public HttpServer addHttpServlet(HttpServlet servlet, String... mappings) {
        this.prepare.addServlet(servlet, null, null, mappings);
        return this;
    }

    /**
     * 添加HttpServlet
     *
     * @param prefix   url前缀
     * @param servlet  HttpServlet
     * @param conf     配置信息
     * @param mappings 匹配规则
     *
     * @return HttpServer
     */
    public HttpServer addHttpServlet(HttpServlet servlet, final String prefix, AnyValue conf, String... mappings) {
        this.prepare.addServlet(servlet, prefix, conf, mappings);
        return this;
    }

    /**
     * 添加WebSocketServlet
     *
     * @param <S>           WebSocket
     * @param <T>           HttpServlet
     * @param classLoader   ClassLoader
     * @param webSocketType WebSocket的类型
     * @param prefix        url前缀
     * @param conf          配置信息
     *
     * @return RestServlet
     */
    public <S extends WebSocket, T extends HttpServlet> T addRestWebSocketServlet(final ClassLoader classLoader, final Class<S> webSocketType, final String prefix, final AnyValue conf) {
        T servlet = Rest.createRestWebSocketServlet(classLoader, webSocketType);
        if (servlet != null) this.prepare.addServlet(servlet, prefix, conf);
        return servlet;
    }

    /**
     * 添加RestServlet
     *
     * @param <S>             Service
     * @param <T>             HttpServlet
     * @param classLoader     ClassLoader
     * @param service         Service对象
     * @param userType        用户数据类型
     * @param baseServletType RestServlet基类
     * @param prefix          url前缀
     *
     * @return RestServlet
     */
    public <S extends Service, T extends HttpServlet> T addRestServlet(final ClassLoader classLoader, final S service, final Class userType, final Class<T> baseServletType, final String prefix) {
        return addRestServlet(classLoader, null, service, userType, baseServletType, prefix);
    }

    /**
     * 添加RestServlet
     *
     * @param <S>             Service
     * @param <T>             HttpServlet
     * @param classLoader     ClassLoader
     * @param name            资源名
     * @param service         Service对象
     * @param userType        用户数据类型
     * @param baseServletType RestServlet基类
     * @param prefix          url前缀
     *
     * @return RestServlet
     */
    @SuppressWarnings("unchecked")
    public <S extends Service, T extends HttpServlet> T addRestServlet(final ClassLoader classLoader, final String name, final S service, final Class userType, final Class<T> baseServletType, final String prefix) {
        T servlet = null;
        final boolean sncp = Sncp.isSncpDyn(service);
        final String resname = name == null ? (sncp ? Sncp.getResourceName(service) : "") : name;
        final Class<S> serviceType = Sncp.getServiceType(service);
        for (final HttpServlet item : ((HttpPrepareServlet) this.prepare).getServlets()) {
            if (!(item instanceof HttpServlet)) continue;
            if (item.getClass().getAnnotation(Rest.RestDyn.class) == null) continue;
            try {
                Field field = item.getClass().getDeclaredField(Rest.REST_SERVICE_FIELD_NAME);
                if (serviceType.equals(field.getType())) {
                    servlet = (T) item;
                    break;
                }
            } catch (NoSuchFieldException | SecurityException e) {
                logger.log(Level.SEVERE, "serviceType = " + serviceType + ", servletClass = " + item.getClass(), e);
            }
        }
        final boolean first = servlet == null;
        if (servlet == null) servlet = Rest.createRestServlet(classLoader, userType, baseServletType, serviceType);
        if (servlet == null) return null; //没有HttpMapping方法的HttpServlet调用Rest.createRestServlet就会返回null 
        try { //若提供动态变更Service服务功能，则改Rest服务无法做出相应更新
            Field field = servlet.getClass().getDeclaredField(Rest.REST_SERVICE_FIELD_NAME);
            field.setAccessible(true);

            Field mapfield = servlet.getClass().getDeclaredField(Rest.REST_SERVICEMAP_FIELD_NAME);
            mapfield.setAccessible(true);

            Service firstService = (Service) field.get(servlet);
            if (resname.isEmpty()) {
                field.set(servlet, service);
                firstService = service;
            }
            Map map = (Map) mapfield.get(servlet);
            if (map == null && !resname.isEmpty()) map = new HashMap();
            if (map != null) {
                map.put(resname, service);
                if (firstService != null) map.put("", firstService);
            }
            mapfield.set(servlet, map);
        } catch (Exception e) {
            throw new RuntimeException(serviceType + " generate rest servlet error", e);
        }
        if (first) this.prepare.addServlet(servlet, prefix, sncp ? Sncp.getConf(service) : null);
        return servlet;
    }

    @Override
    @SuppressWarnings("unchecked")
    protected HttpContext createContext() {
        final int port = this.address.getPort();
        AtomicLong createBufferCounter = new AtomicLong();
        AtomicLong cycleBufferCounter = new AtomicLong();
        this.bufferCapacity = Math.max(this.bufferCapacity, 16 * 1024 + 16); //兼容 HTTP 2.0;
        final int rcapacity = this.bufferCapacity;
        ObjectPool<ByteBuffer> bufferPool = new ObjectPool<>(createBufferCounter, cycleBufferCounter, this.bufferPoolSize,
            (Object... params) -> ByteBuffer.allocateDirect(rcapacity), null, (e) -> {
                if (e == null || e.isReadOnly() || e.capacity() != rcapacity) return false;
                e.clear();
                return true;
            });
        final List<String[]> defaultAddHeaders = new ArrayList<>();
        final List<String[]> defaultSetHeaders = new ArrayList<>();
        boolean autoOptions = false;
        String plainContentType = null;
        String jsonContentType = null;
        HttpCookie defaultCookie = null;
        String remoteAddrHeader = null;
        if (config != null) {
            AnyValue reqs = config == null ? null : config.getAnyValue("request");
            if (reqs != null) {
                AnyValue raddr = reqs.getAnyValue("remoteaddr");
                remoteAddrHeader = raddr == null ? null : raddr.getValue("value");
                if (remoteAddrHeader != null) {
                    if (remoteAddrHeader.startsWith("request.headers.")) {
                        remoteAddrHeader = remoteAddrHeader.substring("request.headers.".length());
                    } else {
                        remoteAddrHeader = null;
                    }
                }
            }

            AnyValue resps = config == null ? null : config.getAnyValue("response");
            if (resps != null) {
                AnyValue contenttypes = resps.getAnyValue("contenttype");
                if (contenttypes != null) {
                    plainContentType = contenttypes.getValue("plain");
                    jsonContentType = contenttypes.getValue("json");
                }
                AnyValue[] addHeaders = resps.getAnyValues("addheader");
                if (addHeaders.length > 0) {
                    for (AnyValue addHeader : addHeaders) {
                        String val = addHeader.getValue("value");
                        if (val == null) continue;
                        if (val.startsWith("request.parameters.")) {
                            defaultAddHeaders.add(new String[]{addHeader.getValue("name"), val, val.substring("request.parameters.".length()), null});
                        } else if (val.startsWith("request.headers.")) {
                            defaultAddHeaders.add(new String[]{addHeader.getValue("name"), val, val.substring("request.headers.".length())});
                        } else if (val.startsWith("system.property.")) {
                            String v = System.getProperty(val.substring("system.property.".length()));
                            if (v != null) defaultAddHeaders.add(new String[]{addHeader.getValue("name"), v});
                        } else {
                            defaultAddHeaders.add(new String[]{addHeader.getValue("name"), val});
                        }
                    }
                }
                AnyValue[] setHeaders = resps.getAnyValues("setheader");
                if (setHeaders.length > 0) {
                    for (AnyValue setHeader : setHeaders) {
                        String val = setHeader.getValue("value");
                        if (val == null) continue;
                        if (val.startsWith("request.parameters.")) {
                            defaultSetHeaders.add(new String[]{setHeader.getValue("name"), val, val.substring("request.parameters.".length()), null});
                        } else if (val.startsWith("request.headers.")) {
                            defaultSetHeaders.add(new String[]{setHeader.getValue("name"), val, val.substring("request.headers.".length())});
                        } else if (val.startsWith("system.property.")) {
                            String v = System.getProperty(val.substring("system.property.".length()));
                            if (v != null) defaultSetHeaders.add(new String[]{setHeader.getValue("name"), v});
                        } else {
                            defaultSetHeaders.add(new String[]{setHeader.getValue("name"), val});
                        }
                    }
                }
                AnyValue defcookieValue = resps.getAnyValue("defcookie");
                if (defcookieValue != null) {
                    String domain = defcookieValue.getValue("domain");
                    String path = defcookieValue.getValue("path");
                    if (domain != null || path != null) {
                        defaultCookie = new HttpCookie("DEFAULTCOOKIE", "");
                        defaultCookie.setDomain(domain);
                        defaultCookie.setPath(path);
                    }
                }
                AnyValue options = resps == null ? null : resps.getAnyValue("options");
                autoOptions = options != null && options.getBoolValue("auto", false);
            }

        }
        final String plainType = plainContentType;
        final String jsonType = jsonContentType;
        final String[][] addHeaders = defaultAddHeaders.isEmpty() ? null : defaultAddHeaders.toArray(new String[defaultAddHeaders.size()][]);
        final String[][] setHeaders = defaultSetHeaders.isEmpty() ? null : defaultSetHeaders.toArray(new String[defaultSetHeaders.size()][]);
        final boolean options = autoOptions;

        final HttpCookie defCookie = defaultCookie;
        final String addrHeader = remoteAddrHeader;
        AtomicLong createResponseCounter = new AtomicLong();
        AtomicLong cycleResponseCounter = new AtomicLong();
        ObjectPool<Response> responsePool = HttpResponse.createPool(createResponseCounter, cycleResponseCounter, this.responsePoolSize, null);
        HttpContext httpcontext = new HttpContext(this.serverStartTime, this.logger, executor, this.sslContext,
            rcapacity, bufferPool, responsePool, this.maxbody, this.charset, this.address, this.resourceFactory,
            this.prepare, this.aliveTimeoutSeconds, this.readTimeoutSeconds, this.writeTimeoutSeconds);
        responsePool.setCreator((Object... params) -> new HttpResponse(httpcontext, new HttpRequest(httpcontext, addrHeader),
            plainType, jsonType, addHeaders, setHeaders, defCookie, options, ((HttpPrepareServlet) prepare).renders));
        return httpcontext;
    }

}
