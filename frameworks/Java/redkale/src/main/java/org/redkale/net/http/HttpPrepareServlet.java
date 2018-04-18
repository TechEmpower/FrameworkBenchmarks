/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import org.redkale.util.AnyValue.DefaultAnyValue;
import java.io.*;
import java.util.*;
import java.util.function.*;
import java.util.logging.*;
import java.util.regex.*;
import org.redkale.net.*;
import org.redkale.net.http.Rest.RestDynSourceType;
import org.redkale.service.Service;
import org.redkale.util.*;

/**
 * HTTP Servlet的总入口，请求在HttpPrepareServlet中进行分流。  <br>
 * 一个HttpServer只有一个HttpPrepareServlet， 用于管理所有HttpServlet。  <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class HttpPrepareServlet extends PrepareServlet<String, HttpContext, HttpRequest, HttpResponse, HttpServlet> {

    protected final Logger logger = Logger.getLogger(this.getClass().getSimpleName());

    protected HttpServlet resourceHttpServlet = new HttpResourceServlet();

    protected MappingEntry[] regArray = null; //regArray 包含 regWsArray

    protected MappingEntry[] regWsArray = null;

    protected Map<String, WebSocketServlet> wsmappings = new HashMap<>(); //super.mappings 包含 wsmappings

    protected final Map<String, Class> allMapStrings = new HashMap<>();

    private final Object excludeLock = new Object();

    private Map<String, BiPredicate<String, String>> forbidURIMaps; //禁用的URL的正则表达式, 必须与 forbidURIPredicates 保持一致

    private BiPredicate<String, String>[] forbidURIPredicates; //禁用的URL的Predicate, 必须与 forbidURIMaps 保持一致

    final List<HttpRender> renders = new ArrayList<>();

    private List<HttpServlet> removeHttpServlet(final Predicate<MappingEntry> predicateEntry, final Predicate<Map.Entry<String, WebSocketServlet>> predicateFilter) {
        List<HttpServlet> servlets = new ArrayList<>();
        synchronized (allMapStrings) {
            List<String> keys = new ArrayList<>();
            if (regArray != null) {
                for (MappingEntry me : regArray) {
                    if (predicateEntry.test(me)) {
                        servlets.add(me.servlet);
                        keys.add(me.mapping);
                    }
                }
            }
            if (regWsArray != null) {
                for (MappingEntry me : regWsArray) {
                    if (predicateEntry.test(me)) {
                        servlets.add(me.servlet);
                        keys.add(me.mapping);
                    }
                }
            }
            Map<String, WebSocketServlet> newwsmappings = new HashMap<>();
            for (Map.Entry<String, WebSocketServlet> en : wsmappings.entrySet()) {
                if (predicateFilter.test(en)) {
                    servlets.add(en.getValue());
                    keys.add(en.getKey());
                } else {
                    newwsmappings.put(en.getKey(), en.getValue());
                }
            }
            if (newwsmappings.size() != wsmappings.size()) this.wsmappings = newwsmappings;
            if (!keys.isEmpty()) {
                this.regArray = Utility.remove(this.regArray, predicateEntry);
                this.regWsArray = Utility.remove(this.regWsArray, predicateEntry);
                for (HttpServlet rs : servlets) {
                    super.removeServlet(rs);
                }
                for (String key : keys) {
                    super.removeMapping(key);
                    allMapStrings.remove(key);
                }
            }
        }
        return servlets;
    }

    public HttpServlet removeHttpServlet(final HttpServlet servlet) {
        Predicate<MappingEntry> predicateEntry = (t) -> t.servlet == servlet;
        Predicate<Map.Entry<String, WebSocketServlet>> predicateFilter = (t) -> t.getValue() == servlet;
        removeHttpServlet(predicateEntry, predicateFilter);
        return servlet;
    }

    public <T extends HttpServlet> HttpServlet removeHttpServlet(Service service) {
        Predicate<MappingEntry> predicateEntry = (t) -> {
            if (!Rest.isRestDyn(t.servlet)) return false;
            Service s = Rest.getService(t.servlet);
            if (s == service) return true;
            if (s != null) return false;
            Map<String, Service> map = Rest.getServiceMap(t.servlet);
            if (map == null) return false;
            boolean rs = map.values().contains(service);
            if (rs && map.size() == 1) return true;
            if (rs && map.size() > 1) {
                String key = null;
                for (Map.Entry<String, Service> en : map.entrySet()) {
                    if (en.getValue() == service) {
                        key = en.getKey();
                        break;
                    }
                }
                if (key != null) map.remove(key);
                return false; //还有其他Resouce.name 的Service
            }
            return rs;
        };
        Predicate<Map.Entry<String, WebSocketServlet>> predicateFilter = null;
        List<HttpServlet> list = removeHttpServlet(predicateEntry, predicateFilter);
        return list == null || list.isEmpty() ? null : list.get(0);
    }
@SuppressWarnings("unchecked")
    public <T extends WebSocket> HttpServlet removeHttpServlet(Class<T> websocketOrServletType) {
        Predicate<MappingEntry> predicateEntry = (t) -> {
            Class type = t.servlet.getClass();
            if (type == websocketOrServletType) return true;
            RestDynSourceType rdt = (RestDynSourceType) type.getAnnotation(RestDynSourceType.class);
            return (rdt != null && rdt.value() == websocketOrServletType);
        };
        Predicate<Map.Entry<String, WebSocketServlet>> predicateFilter = (t) -> {
            Class type = t.getValue().getClass();
            if (type == websocketOrServletType) return true;
            RestDynSourceType rdt = (RestDynSourceType) type.getAnnotation(RestDynSourceType.class);
            return (rdt != null && rdt.value() == websocketOrServletType);
        };
        List<HttpServlet> list = removeHttpServlet(predicateEntry, predicateFilter);
        return list == null || list.isEmpty() ? null : list.get(0);
    }

    @SuppressWarnings("unchecked")
    public boolean addForbidURIReg(final String urlreg) {
        if (urlreg == null || urlreg.isEmpty()) return false;
        synchronized (excludeLock) {
            if (forbidURIMaps != null && forbidURIMaps.containsKey(urlreg)) return false;
            if (forbidURIMaps == null) forbidURIMaps = new HashMap<>();
            String mapping = urlreg;
            if (Utility.contains(mapping, '.', '*', '{', '[', '(', '|', '^', '$', '+', '?', '\\')) { //是否是正则表达式))
                if (mapping.endsWith("/*")) {
                    mapping = mapping.substring(0, mapping.length() - 1) + ".*";
                } else {
                    mapping = mapping + "$";
                }
            }
            final String reg = mapping;
            final boolean begin = mapping.charAt(0) == '^';
            final Predicate regPredicate = Pattern.compile(reg).asPredicate();
            BiPredicate<String, String> predicate = (prefix, uri) -> {
                return begin || prefix.isEmpty() ? regPredicate.test(uri) : uri.matches(prefix + reg);
            };
            forbidURIMaps.put(urlreg, predicate);
            forbidURIPredicates = Utility.append(forbidURIPredicates, predicate);
            return true;
        }
    }

    @SuppressWarnings("unchecked")
    public boolean removeForbidURIReg(final String urlreg) {
        if (urlreg == null || urlreg.isEmpty()) return false;
        synchronized (excludeLock) {
            if (forbidURIMaps == null || forbidURIPredicates == null || !forbidURIMaps.containsKey(urlreg)) return false;
            BiPredicate<String, String> predicate = forbidURIMaps.get(urlreg);
            forbidURIMaps.remove(urlreg);
            int index = -1;
            for (int i = 0; i < forbidURIPredicates.length; i++) {
                if (forbidURIPredicates[i] == predicate) {
                    index = i;
                    break;
                }
            }
            if (index > -1) {
                if (forbidURIPredicates.length == 1) {
                    forbidURIPredicates = null;
                } else {
                    int newlen = forbidURIPredicates.length - 1;
                    BiPredicate[] news = new BiPredicate[newlen];
                    System.arraycopy(forbidURIPredicates, 0, news, 0, index);
                    System.arraycopy(forbidURIPredicates, index + 1, news, index, newlen - index);
                    forbidURIPredicates = news;
                }
            }
            return true;
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public void init(HttpContext context, AnyValue config) {
        super.init(context, config); //必须要执行
        Collection<HttpServlet> servlets = getServlets();
        servlets.forEach(s -> {
            s.preInit(context, getServletConf(s));
            s.init(context, getServletConf(s));
        });
        { //设置ResourceServlet
            AnyValue resConfig = config.getAnyValue("resource-servlet");
            if ((resConfig instanceof DefaultAnyValue) && resConfig.getValue("webroot", "").isEmpty()) {
                ((DefaultAnyValue) resConfig).addValue("webroot", config.getValue("root"));
            }
            if (resConfig == null) { //主要用于嵌入式的HttpServer初始化
                DefaultAnyValue dresConfig = new DefaultAnyValue();
                dresConfig.addValue("webroot", config.getValue("root"));
                dresConfig.addValue("ranges", config.getValue("ranges"));
                dresConfig.addValue("cache", config.getAnyValue("cache"));
                AnyValue[] rewrites = config.getAnyValues("rewrite");
                if (rewrites != null) {
                    for (AnyValue rewrite : rewrites) {
                        dresConfig.addValue("rewrite", rewrite);
                    }
                }
                resConfig = dresConfig;
            }
            String resServlet = resConfig.getValue("servlet", HttpResourceServlet.class.getName());
            try {
                this.resourceHttpServlet = (HttpServlet) Thread.currentThread().getContextClassLoader().loadClass(resServlet).getDeclaredConstructor().newInstance();
            } catch (Throwable e) {
                this.resourceHttpServlet = new HttpResourceServlet();
                logger.log(Level.WARNING, "init HttpResourceSerlvet(" + resServlet + ") error", e);
            }
            context.getResourceFactory().inject(this.resourceHttpServlet);
            this.resourceHttpServlet.init(context, resConfig);
        }
        { //设置TemplateEngine            
            AnyValue[] renderConfigs = config.getAnyValues("render");
            if (renderConfigs != null) {
                for (AnyValue renderConfig : renderConfigs) {
                    String renderType = renderConfig.getValue("value");
                    try {
                        HttpRender render = (HttpRender) Thread.currentThread().getContextClassLoader().loadClass(renderType).getDeclaredConstructor().newInstance();
                        for (HttpRender one : renders) {
                            if (one.getType().equals(render.getType())) throw new RuntimeException("HttpRender(" + renderType + ") repeat");
                        }
                        context.getResourceFactory().inject(render);
                        render.init(context, renderConfig);
                        renders.add(render);
                    } catch (Throwable e) {
                        logger.log(Level.WARNING, "init HttpRender(" + renderType + ") error", e);
                    }
                }
                Collections.sort(renders, (o1, o2) -> o1.getType().isAssignableFrom(o2.getType()) ? 1 : -1);
            }
        }
    }

    @Override
    public void execute(HttpRequest request, HttpResponse response) throws IOException {
        try {
            final String uri = request.getRequestURI();
            HttpServlet servlet;
            if (response.isAutoOptions() && "OPTIONS".equals(request.getMethod())) {
                response.finish(200, null);
                return;
            }
            if (request.isWebSocket()) {
                servlet = wsmappings.get(uri);
                if (servlet == null && this.regWsArray != null) {
                    for (MappingEntry en : regWsArray) {
                        if (en.predicate.test(uri)) {
                            servlet = en.servlet;
                            break;
                        }
                    }
                }
                if (servlet == null) {
                    response.finish(500, null);
                    return;
                }
            } else {
                servlet = mappingServlet(uri);
                if (servlet == null && this.regArray != null) {
                    for (MappingEntry en : regArray) {
                        if (en.predicate.test(uri)) {
                            servlet = en.servlet;
                            break;
                        }
                    }
                }
                //找不到匹配的HttpServlet则使用静态资源HttpResourceServlet
                if (servlet == null) servlet = this.resourceHttpServlet;
            }
            boolean forbid = false;
            BiPredicate<String, String>[] forbidUrlPredicates = this.forbidURIPredicates;
            if (forbidUrlPredicates != null && forbidUrlPredicates.length > 0) {
                for (BiPredicate<String, String> predicate : forbidUrlPredicates) {
                    if (predicate != null && predicate.test(servlet._prefix, uri)) {
                        forbid = true;
                        break;
                    }
                }
            }
            if (forbid) {
                response.finish(403, response.getHttpCode(403));
                return;
            }
            servlet.execute(request, response);
        } catch (Exception e) {
            request.getContext().getLogger().log(Level.WARNING, "Servlet occur, forece to close channel. request = " + request, e);
            response.finish(500, null);
        }
    }

    /**
     * 添加HttpServlet
     *
     * @param servlet  HttpServlet
     * @param prefix   url前缀
     * @param conf     配置信息
     * @param mappings 匹配规则
     */
    @Override
    public void addServlet(HttpServlet servlet, Object prefix, AnyValue conf, String... mappings) {
        if (prefix == null) prefix = "";
        if (mappings.length < 1) {
            WebServlet ws = servlet.getClass().getAnnotation(WebServlet.class);
            if (ws != null) {
                mappings = ws.value();
                if (!ws.repair()) prefix = "";//被设置为不自动追加前缀则清空prefix
            }
        }
        synchronized (allMapStrings) {  //需要整段锁住
            for (String mapping : mappings) {
                if (mapping == null) continue;
                if (!prefix.toString().isEmpty()) mapping = prefix + mapping;

                if (Utility.contains(mapping, '.', '*', '{', '[', '(', '|', '^', '$', '+', '?', '\\')) { //是否是正则表达式))
                    if (mapping.charAt(0) != '^') mapping = '^' + mapping;
                    if (mapping.endsWith("/*")) {
                        mapping = mapping.substring(0, mapping.length() - 1) + ".*";
                    } else {
                        mapping = mapping + "$";
                    }
                    if (regArray == null) {
                        regArray = new MappingEntry[1];
                        regArray[0] = new MappingEntry(mapping, Pattern.compile(mapping).asPredicate(), servlet);
                    } else {
                        regArray = Arrays.copyOf(regArray, regArray.length + 1);
                        regArray[regArray.length - 1] = new MappingEntry(mapping, Pattern.compile(mapping).asPredicate(), servlet);
                    }
                    if (servlet instanceof WebSocketServlet) {
                        if (regWsArray == null) {
                            regWsArray = new MappingEntry[1];
                            regWsArray[0] = new MappingEntry(mapping, Pattern.compile(mapping).asPredicate(), (WebSocketServlet) servlet);
                        } else {
                            regWsArray = Arrays.copyOf(regWsArray, regWsArray.length + 1);
                            regWsArray[regWsArray.length - 1] = new MappingEntry(mapping, Pattern.compile(mapping).asPredicate(), (WebSocketServlet) servlet);
                        }
                    }
                } else if (mapping != null && !mapping.isEmpty()) {
                    putMapping(mapping, servlet);
                    if (servlet instanceof WebSocketServlet) {
                        Map<String, WebSocketServlet> newmappings = new HashMap<>(wsmappings);
                        newmappings.put(mapping, (WebSocketServlet) servlet);
                        this.wsmappings = newmappings;
                    }
                }
                if (this.allMapStrings.containsKey(mapping)) {
                    Class old = this.allMapStrings.get(mapping);
                    throw new RuntimeException("mapping [" + mapping + "] repeat on " + old.getName() + " and " + servlet.getClass().getName());
                }
                this.allMapStrings.put(mapping, servlet.getClass());
            }
            setServletConf(servlet, conf);
            servlet._prefix = prefix.toString();
            putServlet(servlet);
        }
    }

    /**
     * 设置静态资源HttpServlet
     *
     * @param servlet HttpServlet
     */
    public void setResourceServlet(HttpServlet servlet) {
        if (servlet != null) {
            this.resourceHttpServlet = servlet;
        }
    }

    /**
     * 获取静态资源HttpServlet
     *
     * @return HttpServlet
     */
    public HttpServlet getResourceServlet() {
        return this.resourceHttpServlet;
    }

    @Override
    public void destroy(HttpContext context, AnyValue config) {
        super.destroy(context, config); //必须要执行
        this.resourceHttpServlet.destroy(context, config);
        getServlets().forEach(s -> {
            s.destroy(context, getServletConf(s));
            s.postDestroy(context, getServletConf(s));
        });
        this.allMapStrings.clear();
        this.wsmappings.clear();
        this.regArray = null;
        this.regWsArray = null;
    }

    protected static class MappingEntry {

        public final String mapping;

        public final Predicate<String> predicate;

        public final HttpServlet servlet;

        public MappingEntry(String mapping, Predicate<String> predicate, HttpServlet servlet) {
            this.mapping = mapping;
            this.predicate = predicate;
            this.servlet = servlet;
        }

    }
}
