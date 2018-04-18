/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.boot;

import java.io.*;
import java.lang.reflect.*;
import java.util.*;
import javax.persistence.*;
import org.redkale.convert.json.JsonConvert;
import org.redkale.net.http.*;
import org.redkale.source.*;
import org.redkale.util.*;

/**
 * API接口文档生成类，作用：生成Application实例中所有HttpServer的可用HttpServlet的API接口方法   <br>
 * 继承 HttpBaseServlet 是为了获取 WebMapping 信息
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public final class ApiDocsService {

    private final Application app; //Application全局对象

    public ApiDocsService(Application app) {
        this.app = app;
    }

    public void run() throws Exception {
        List<Map> serverList = new ArrayList<>();
        Field __prefix = HttpServlet.class.getDeclaredField("_prefix");
        __prefix.setAccessible(true);
        Map<String, Map<String, Map<String, Object>>> typesmap = new LinkedHashMap<>();
        for (NodeServer node : app.servers) {
            if (!(node instanceof NodeHttpServer)) continue;
            final Map<String, Object> map = new LinkedHashMap<>();
            serverList.add(map);
            HttpServer server = node.getServer();
            map.put("address", server.getSocketAddress());
            List<Map<String, Object>> servletsList = new ArrayList<>();
            map.put("servlets", servletsList);
            for (HttpServlet servlet : server.getPrepareServlet().getServlets()) {
                if (!(servlet instanceof HttpServlet)) continue;
                WebServlet ws = servlet.getClass().getAnnotation(WebServlet.class);
                if (ws == null) {
                    System.err.println(servlet + " not found @WebServlet");
                    continue;
                }
                final Map<String, Object> servletmap = new LinkedHashMap<>();
                String prefix = (String) __prefix.get(servlet);
                String[] urlregs = ws.value();
                if (prefix != null && !prefix.isEmpty()) {
                    for (int i = 0; i < urlregs.length; i++) {
                        urlregs[i] = prefix + urlregs[i];
                    }
                }
                servletmap.put("urlregs", urlregs);
                servletmap.put("moduleid", ws.moduleid());
                servletmap.put("name", ws.name());
                servletmap.put("comment", ws.comment());

                List<Map> mappingsList = new ArrayList<>();
                servletmap.put("mappings", mappingsList);
                final Class selfClz = servlet.getClass();
                Class clz = servlet.getClass();
                HashSet<String> actionurls = new HashSet<>();
                do {
                    if (Modifier.isAbstract(clz.getModifiers())) break;
                    for (Method method : clz.getMethods()) {
                        if (method.getParameterCount() != 2) continue;
                        HttpMapping action = method.getAnnotation(HttpMapping.class);
                        if (action == null) continue;
                        if (!action.inherited() && selfClz != clz) continue; //忽略不被继承的方法
                        final Map<String, Object> mappingmap = new LinkedHashMap<>();
                        if (actionurls.contains(action.url())) continue;
                        mappingmap.put("url", prefix + action.url());
                        actionurls.add(action.url());
                        mappingmap.put("auth", action.auth());
                        mappingmap.put("actionid", action.actionid());
                        mappingmap.put("comment", action.comment());
                        List<Map> paramsList = new ArrayList<>();
                        mappingmap.put("params", paramsList);
                        List<String> results = new ArrayList<>();
                        for (final Class rtype : action.results()) {
                            results.add(rtype.getName());
                            if (typesmap.containsKey(rtype.getName())) continue;
                            final boolean filter = FilterBean.class.isAssignableFrom(rtype);
                            final Map<String, Map<String, Object>> typemap = new LinkedHashMap<>();
                            Class loop = rtype;
                            do {
                                if (loop == null || loop.isInterface()) break;
                                for (Field field : loop.getDeclaredFields()) {
                                    if (Modifier.isFinal(field.getModifiers())) continue;
                                    if (Modifier.isStatic(field.getModifiers())) continue;

                                    Map<String, Object> fieldmap = new LinkedHashMap<>();
                                    fieldmap.put("type", field.getType().isArray() ? (field.getType().getComponentType().getName() + "[]") : field.getGenericType().getTypeName());

                                    Comment comment = field.getAnnotation(Comment.class);
                                    Column col = field.getAnnotation(Column.class);
                                    FilterColumn fc = field.getAnnotation(FilterColumn.class);
                                    if (comment != null) {
                                        fieldmap.put("comment", comment.value());
                                    } else if (col != null) {
                                        fieldmap.put("comment", col.comment());
                                    } else if (fc != null) {
                                        fieldmap.put("comment", fc.comment());
                                    }
                                    fieldmap.put("primary", !filter && (field.getAnnotation(Id.class) != null));
                                    fieldmap.put("updatable", (filter || col == null || col.updatable()));
                                    if (servlet.getClass().getAnnotation(Rest.RestDyn.class) != null) {
                                        if (field.getAnnotation(RestAddress.class) != null) continue;
                                    }

                                    typemap.put(field.getName(), fieldmap);
                                }
                            } while ((loop = loop.getSuperclass()) != Object.class);
                            typesmap.put(rtype.getName(), typemap);
                        }
                        mappingmap.put("results", results);
                        for (HttpParam param : method.getAnnotationsByType(HttpParam.class)) {
                            final Map<String, Object> parammap = new LinkedHashMap<>();
                            final boolean isarray = param.type().isArray();
                            final Class ptype = isarray ? param.type().getComponentType() : param.type();
                            parammap.put("name", param.name());
                            parammap.put("radix", param.radix());
                            parammap.put("type", ptype.getName() + (isarray ? "[]" : ""));
                            parammap.put("src", param.src());
                            parammap.put("comment", param.comment());
                            parammap.put("required", param.required());
                            paramsList.add(parammap);
                            if (ptype.isPrimitive() || ptype == String.class) continue;
                            if (typesmap.containsKey(ptype.getName())) continue;

                            final Map<String, Map<String, Object>> typemap = new LinkedHashMap<>();
                            Class loop = ptype;
                            final boolean filter = FilterBean.class.isAssignableFrom(loop);
                            do {
                                if (loop == null || loop.isInterface()) break;
                                for (Field field : loop.getDeclaredFields()) {
                                    if (Modifier.isFinal(field.getModifiers())) continue;
                                    if (Modifier.isStatic(field.getModifiers())) continue;

                                    Map<String, Object> fieldmap = new LinkedHashMap<>();
                                    fieldmap.put("type", field.getType().isArray() ? (field.getType().getComponentType().getName() + "[]") : field.getGenericType().getTypeName());

                                    Column col = field.getAnnotation(Column.class);
                                    FilterColumn fc = field.getAnnotation(FilterColumn.class);
                                    Comment comment = field.getAnnotation(Comment.class);
                                    if (comment != null) {
                                        fieldmap.put("comment", comment.value());
                                    } else if (col != null) {
                                        fieldmap.put("comment", col.comment());
                                    } else if (fc != null) {
                                        fieldmap.put("comment", fc.comment());
                                    }
                                    fieldmap.put("primary", !filter && (field.getAnnotation(Id.class) != null));
                                    fieldmap.put("updatable", (filter || col == null || col.updatable()));

                                    if (servlet.getClass().getAnnotation(Rest.RestDyn.class) != null) {
                                        if (field.getAnnotation(RestAddress.class) != null) continue;
                                    }

                                    typemap.put(field.getName(), fieldmap);
                                }
                            } while ((loop = loop.getSuperclass()) != Object.class);

                            typesmap.put(ptype.getName(), typemap);
                        }
                        mappingmap.put("result", action.result());
                        mappingsList.add(mappingmap);
                    }
                } while ((clz = clz.getSuperclass()) != HttpServlet.class);
                mappingsList.sort((o1, o2) -> ((String) o1.get("url")).compareTo((String) o2.get("url")));
                servletsList.add(servletmap);
            }
            servletsList.sort((o1, o2) -> {
                String[] urlregs1 = (String[]) o1.get("urlregs");
                String[] urlregs2 = (String[]) o2.get("urlregs");
                return urlregs1.length > 0 ? (urlregs2.length > 0 ? urlregs1[0].compareTo(urlregs2[0]) : 1) : -1;
            });
        }
        Map<String, Object> resultmap = new LinkedHashMap<>();
        resultmap.put("servers", serverList);
        resultmap.put("types", typesmap);
        final String json = JsonConvert.root().convertTo(resultmap);
        final FileOutputStream out = new FileOutputStream(new File(app.getHome(), "apidoc.json"));
        out.write(json.getBytes("UTF-8"));
        out.close();
        File doctemplate = new File(app.getHome(), "conf/apidoc-template.html");
        InputStream in = null;
        if (doctemplate.isFile() && doctemplate.canRead()) {
            in = new FileInputStream(doctemplate);
        }
        if (in == null) in = ApiDocsService.class.getResourceAsStream("apidoc-template.html");
        String content = Utility.read(in).replace("'${content}'", json);
        in.close();
        FileOutputStream outhtml = new FileOutputStream(new File(app.getHome(), "apidoc.html"));
        outhtml.write(content.getBytes("UTF-8"));
        outhtml.close();
    }

}
