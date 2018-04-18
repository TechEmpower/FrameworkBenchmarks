/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this referid file, choose Tools | Templates
 * and open the referid in the editor.
 */
package org.redkale.net.http;

import java.util.*;
import java.util.function.BiConsumer;
import org.redkale.convert.*;
import org.redkale.convert.json.JsonConvert;

/**
 * HTTP输出引擎的对象域 <br>
 * 输出引擎的核心类, 业务开发人员只有通过本类对象才能调用到输出引擎功能。 <br>
 * <p>
 * HttpServlet调用: <br>
 * <pre>
 *    &#064;HttpMapping(url = "/hello.html", auth = false)
 *    public void hello(HttpRequest req, HttpResponse resp) throws IOException {
 *        resp.finish(HttpScope.refer("/hello.html").attr("content", "哈哈"));
 *    }
 * </pre>
 * <p>
 * RestService调用: <br>
 * <pre>
 *    &#064;RestMapping(name = "hello.html", auth = false)
 *    public HttpScope hello() {
 *       return HttpScope.refer("hello.html").attr("content", "哈哈");
 *    }
 * </pre>
 *
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class HttpScope {

    protected String referid;

    protected Map<String, Object> attributes;

    public static HttpScope refer(String template) {
        HttpScope rs = new HttpScope();
        rs.setReferid(template);
        return rs;
    }

    public HttpScope attr(Map<String, Object> map) {
        if (map == null) return this;
        if (this.attributes == null) this.attributes = new LinkedHashMap<>();
        this.attributes.putAll(map);
        return this;
    }

    public HttpScope attr(String name, Object value) {
        if (this.attributes == null) this.attributes = new LinkedHashMap<>();
        this.attributes.put(name, value);
        return this;
    }

    @SuppressWarnings("unchecked")
    public <T> T find(String name) {
        return this.attributes == null ? null : (T) this.attributes.get(name);
    }

    @SuppressWarnings("unchecked")
    public <T> T find(HttpScope parent, String name) {
        T rs = this.attributes == null ? null : (T) this.attributes.get(name);
        if (rs != null) return rs;
        return parent == null ? null : parent.find(name);
    }

    public void forEach(BiConsumer<String, Object> action) {
        if (this.attributes == null) return;
        this.attributes.forEach(action);
    }

    public String getReferid() {
        return referid;
    }

    public void setReferid(String referid) {
        this.referid = referid;
    }

    public Map<String, Object> getAttributes() {
        return attributes;
    }

    @ConvertDisabled(type = ConvertType.JSON)
    public void setAttributes(Map<String, Object> attributes) {
        this.attributes = attributes;
    }

    @Override
    public String toString() {
        return JsonConvert.root().convertTo(this);
    }
}
