/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.ByteBuffer;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiFunction;
import org.redkale.asm.*;
import static org.redkale.asm.ClassWriter.COMPUTE_FRAMES;
import static org.redkale.asm.Opcodes.*;
import org.redkale.net.*;
import org.redkale.service.RetResult;
import org.redkale.util.*;

/**
 * HTTP版的Servlet， 执行顺序 execute --&#62; preExecute --&#62; authenticate --&#62; HttpMapping对应的方法
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class HttpServlet extends Servlet<HttpContext, HttpRequest, HttpResponse> {

    public static final int RET_SERVER_ERROR = 1200_0001;

    public static final int RET_METHOD_ERROR = 1200_0002;

    String _prefix = ""; //当前HttpServlet的path前缀

    private Map.Entry<String, Entry>[] mappings;

    //这里不能直接使用HttpServlet，会造成死循环初始化HttpServlet
    private final Servlet<HttpContext, HttpRequest, HttpResponse> authSuccessServlet = new Servlet<HttpContext, HttpRequest, HttpResponse>() {
        @Override
        public void execute(HttpRequest request, HttpResponse response) throws IOException {
            Entry entry = (Entry) request.attachment;
            if (entry.cacheseconds > 0) {//有缓存设置
                CacheEntry ce = entry.cache.get(request.getRequestURI());
                if (ce != null && ce.time + entry.cacheseconds > System.currentTimeMillis()) { //缓存有效
                    response.setStatus(ce.status);
                    response.setContentType(ce.contentType);
                    response.finish(ce.getBuffers());
                    return;
                }
                response.setBufferHandler(entry.cacheHandler);
            }
            entry.servlet.execute(request, response);
        }
    };

    //preExecute运行完后执行的Servlet
    private final Servlet<HttpContext, HttpRequest, HttpResponse> preSuccessServlet = new Servlet<HttpContext, HttpRequest, HttpResponse>() {
        @Override
        public void execute(HttpRequest request, HttpResponse response) throws IOException {
            for (Map.Entry<String, Entry> en : mappings) {
                if (request.getRequestURI().startsWith(en.getKey())) {
                    Entry entry = en.getValue();
                    if (!entry.checkMethod(request.getMethod())) {
                        response.finishJson(new RetResult(RET_METHOD_ERROR, "Method(" + request.getMethod() + ") Error"));
                        return;
                    }
                    request.attachment = entry;
                    request.moduleid = entry.moduleid;
                    request.actionid = entry.actionid;
                    if (entry.ignore) {
                        authSuccessServlet.execute(request, response);
                    } else {
                        response.thenEvent(authSuccessServlet);
                        authenticate(request, response);
                    }
                    return;
                }
            }
            throw new IOException(this.getClass().getName() + " not found method for URI(" + request.getRequestURI() + ")");
        }
    };

    @SuppressWarnings("unchecked")
    void preInit(HttpContext context, AnyValue config) {
        String path = _prefix == null ? "" : _prefix;
        WebServlet ws = this.getClass().getAnnotation(WebServlet.class);
        if (ws != null && !ws.repair()) path = "";
        HashMap<String, Entry> map = load();
        this.mappings = new Map.Entry[map.size()];
        int i = -1;
        for (Map.Entry<String, Entry> en : map.entrySet()) {
            mappings[++i] = new AbstractMap.SimpleEntry<>(path + en.getKey(), en.getValue());
        }
        //必须要倒排序, /query /query1 /query12  确保含子集的优先匹配 /query12  /query1  /query
        Arrays.sort(mappings, (o1, o2) -> o2.getKey().compareTo(o1.getKey()));
    }

    void postDestroy(HttpContext context, AnyValue config) {
    }

    /**
     * <p>
     * 预执行方法，在execute方法之前运行，设置当前用户信息，或者加入常规统计和基础检测，例如 : <br>
     * <blockquote><pre>
     *      &#64;Override
     *      public void preExecute(final HttpRequest request, final HttpResponse response) throws IOException {
     *          //设置当前用户信息
     *          final String sessionid = request.getSessionid(false);
     *          if (sessionid != null) request.setCurrentUser(userService.current(sessionid));
     *
     *          if (finer) response.recycleListener((req, resp) -&#62; {  //记录处理时间比较长的请求
     *              long e = System.currentTimeMillis() - ((HttpRequest) req).getCreatetime();
     *              if (e &#62; 200) logger.finer("http-execute-cost-time: " + e + " ms. request = " + req);
     *          });
     *          response.nextEvent();
     *      }
     * </pre></blockquote>
     * <p>
     *
     * @param request  HttpRequest
     * @param response HttpResponse
     *
     * @throws IOException IOException
     */
    protected void preExecute(HttpRequest request, HttpResponse response) throws IOException {
        response.nextEvent();
    }

    /**
     * <p>
     * 用户登录或权限验证， 注解为&#64;HttpMapping.auth == true 的方法会执行authenticate方法, 若验证成功则必须调用response.nextEvent();进行下一步操作, 例如: <br>
     * <blockquote><pre>
     *      &#64;Override
     *      public void authenticate(HttpRequest request, HttpResponse response) throws IOException {
     *          UserInfo info = request.currentUser();
     *          if (info == null) {
     *              response.finishJson(RET_UNLOGIN);
     *              return;
     *          } else if (!info.checkAuth(request.getModuleid(), request.getActionid())) {
     *              response.finishJson(RET_AUTHILLEGAL);
     *              return;
     *          }
     *          response.nextEvent();
     *      }
     * </pre></blockquote>
     * <p>
     *
     *
     * @param request  HttpRequest
     * @param response HttpResponse
     *
     * @throws IOException IOException
     */
    protected void authenticate(HttpRequest request, HttpResponse response) throws IOException {
        response.nextEvent();
    }

    @Override
    public void execute(HttpRequest request, HttpResponse response) throws IOException {
        response.thenEvent(preSuccessServlet);
        preExecute(request, response);
    }

    private HashMap<String, Entry> load() {
        WebServlet module = this.getClass().getAnnotation(WebServlet.class);
        final int serviceid = module == null ? 0 : module.moduleid();
        final HashMap<String, Entry> map = new HashMap<>();
        HashMap<String, Class> nameset = new HashMap<>();
        final Class selfClz = this.getClass();
        Class clz = this.getClass();
        do {
            if (java.lang.reflect.Modifier.isAbstract(clz.getModifiers())) break;
            for (final Method method : clz.getMethods()) {
                //-----------------------------------------------
                String methodname = method.getName();
                if ("service".equals(methodname) || "preExecute".equals(methodname) || "execute".equals(methodname) || "authenticate".equals(methodname)) continue;
                //-----------------------------------------------
                Class[] paramTypes = method.getParameterTypes();
                if (paramTypes.length != 2 || paramTypes[0] != HttpRequest.class
                    || paramTypes[1] != HttpResponse.class) continue;
                //-----------------------------------------------
                Class[] exps = method.getExceptionTypes();
                if (exps.length > 0 && (exps.length != 1 || exps[0] != IOException.class)) continue;
                //-----------------------------------------------

                final HttpMapping mapping = method.getAnnotation(HttpMapping.class);
                if (mapping == null) continue;
                final boolean inherited = mapping.inherited();
                if (!inherited && selfClz != clz) continue; //忽略不被继承的方法
                final int actionid = mapping.actionid();
                final String name = mapping.url().trim();
                final String[] methods = mapping.methods();
                if (nameset.containsKey(name)) {
                    if (nameset.get(name) != clz) continue;
                    throw new RuntimeException(this.getClass().getSimpleName() + " have two same " + HttpMapping.class.getSimpleName() + "(" + name + ")");
                }
                nameset.put(name, clz);
                map.put(name, new Entry(serviceid, actionid, name, methods, method, createHttpServlet(method)));
            }
        } while ((clz = clz.getSuperclass()) != HttpServlet.class);
        return map;
    }

    private HttpServlet createHttpServlet(final Method method) {
        //------------------------------------------------------------------------------
        final String supDynName = HttpServlet.class.getName().replace('.', '/');
        final String interName = this.getClass().getName().replace('.', '/');
        final String interDesc = org.redkale.asm.Type.getDescriptor(this.getClass());
        final String requestSupDesc = org.redkale.asm.Type.getDescriptor(Request.class);
        final String responseSupDesc = org.redkale.asm.Type.getDescriptor(Response.class);
        final String requestDesc = org.redkale.asm.Type.getDescriptor(HttpRequest.class);
        final String responseDesc = org.redkale.asm.Type.getDescriptor(HttpResponse.class);
        String newDynName = interName + "_Dyn_" + method.getName();
        int i = 0;
        for (;;) {
            try {
                Thread.currentThread().getContextClassLoader().loadClass(newDynName.replace('/', '.'));
                newDynName += "_" + (++i);
            } catch (Throwable ex) {
                break;
            }
        }
        //------------------------------------------------------------------------------
        ClassWriter cw = new ClassWriter(COMPUTE_FRAMES);
        FieldVisitor fv;
        MethodVisitor mv;
        AnnotationVisitor av0;
        final String factfield = "_factServlet";
        cw.visit(V1_8, ACC_PUBLIC + ACC_FINAL + ACC_SUPER, newDynName, null, supDynName, null);
        {
            fv = cw.visitField(ACC_PUBLIC, factfield, interDesc, null, null);
            fv.visitEnd();
        }
        { //构造函数
            mv = (cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null));
            //mv.setDebug(true);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, supDynName, "<init>", "()V", false);
            mv.visitInsn(RETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }
        {
            mv = (cw.visitMethod(ACC_PUBLIC, "execute", "(" + requestDesc + responseDesc + ")V", null, new String[]{"java/io/IOException"}));
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, newDynName, factfield, interDesc);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitVarInsn(ALOAD, 2);
            mv.visitMethodInsn(INVOKEVIRTUAL, interName, method.getName(), "(" + requestDesc + responseDesc + ")V", false);
            mv.visitInsn(RETURN);
            mv.visitMaxs(3, 3);
            mv.visitEnd();
        }
        {
            mv = cw.visitMethod(ACC_PUBLIC + ACC_BRIDGE + ACC_SYNTHETIC, "execute", "(" + requestSupDesc + responseSupDesc + ")V", null, new String[]{"java/io/IOException"});
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitTypeInsn(CHECKCAST, HttpRequest.class.getName().replace('.', '/'));
            mv.visitVarInsn(ALOAD, 2);
            mv.visitTypeInsn(CHECKCAST, HttpResponse.class.getName().replace('.', '/'));
            mv.visitMethodInsn(INVOKEVIRTUAL, newDynName, "execute", "(" + requestDesc + responseDesc + ")V", false);
            mv.visitInsn(RETURN);
            mv.visitMaxs(3, 3);
            mv.visitEnd();
        }
        cw.visitEnd();
        //------------------------------------------------------------------------------
        byte[] bytes = cw.toByteArray();
        Class<?> newClazz = new ClassLoader(this.getClass().getClassLoader()) {
            public final Class<?> loadClass(String name, byte[] b) {
                return defineClass(name, b, 0, b.length);
            }
        }.loadClass(newDynName.replace('/', '.'), bytes);
        try {
            HttpServlet instance = (HttpServlet) newClazz.getDeclaredConstructor().newInstance();
            instance.getClass().getField(factfield).set(instance, this);
            return instance;
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    private static final class Entry {

        public Entry(int moduleid, int actionid, String name, String[] methods, Method method, HttpServlet servlet) {
            this.moduleid = moduleid;
            this.actionid = actionid;
            this.name = name;
            this.methods = methods;
            this.method = method;
            this.servlet = servlet;
            HttpMapping mapping = method.getAnnotation(HttpMapping.class);
            this.ignore = mapping == null || !mapping.auth();
            this.cacheseconds = mapping == null ? 0 : mapping.cacheseconds();
            this.cache = cacheseconds > 0 ? new ConcurrentHashMap<>() : null;
            this.cacheHandler = cacheseconds > 0 ? (HttpResponse response, ByteBuffer[] buffers) -> {
                int status = response.getStatus();
                if (status != 200) return null;
                CacheEntry ce = new CacheEntry(response.getStatus(), response.getContentType(), buffers);
                cache.put(response.getRequest().getRequestURI(), ce);
                return ce.getBuffers();
            } : null;
        }

        public boolean isNeedCheck() {
            return this.moduleid != 0 || this.actionid != 0;
        }

        public boolean checkMethod(final String reqMethod) {
            if (methods.length == 0) return true;
            for (String m : methods) {
                if (reqMethod.equalsIgnoreCase(m)) return true;
            }
            return false;
        }

        public final BiFunction<HttpResponse, ByteBuffer[], ByteBuffer[]> cacheHandler;

        public final ConcurrentHashMap<String, CacheEntry> cache;

        public final int cacheseconds;

        public final boolean ignore;

        public final int moduleid;

        public final int actionid;

        public final String name;

        public final String[] methods;

        public final Method method;

        public final HttpServlet servlet;
    }

    private static final class CacheEntry {

        public final long time = System.currentTimeMillis();

        private final ByteBuffer[] buffers;

        private final int status;

        private final String contentType;

        public CacheEntry(int status, String contentType, ByteBuffer[] bufs) {
            this.status = status;
            this.contentType = contentType;
            final ByteBuffer[] newBuffers = new ByteBuffer[bufs.length];
            for (int i = 0; i < newBuffers.length; i++) {
                newBuffers[i] = bufs[i].duplicate().asReadOnlyBuffer();
            }
            this.buffers = newBuffers;
        }

        public ByteBuffer[] getBuffers() {
            final ByteBuffer[] newBuffers = new ByteBuffer[buffers.length];
            for (int i = 0; i < newBuffers.length; i++) {
                newBuffers[i] = buffers[i].duplicate();
            }
            return newBuffers;
        }
    }
}
