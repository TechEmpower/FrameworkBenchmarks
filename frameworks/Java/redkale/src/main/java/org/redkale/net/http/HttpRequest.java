/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import java.io.*;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.charset.*;
import java.util.*;
import org.redkale.convert.json.JsonConvert;
import org.redkale.net.*;
import org.redkale.util.*;
import org.redkale.util.AnyValue.DefaultAnyValue;

/**
 * Http请求包 与javax.servlet.http.HttpServletRequest 基本类似。  <br>
 * 同时提供json的解析接口: public Object getJsonParameter(Type type, String name)  <br>
 * Redkale提倡带简单的参数的GET请求采用类似REST风格, 因此提供了 getRequstURIPath 系列接口。  <br>
 * 例如简单的翻页查询   <br>
 *      /pipes/record/query/offset:0/limit:20 <br>
 * 获取页号: int offset = request.getRequstURIPath("offset:", 0);   <br>
 * 获取行数: int limit = request.getRequstURIPath("limit:", 10);  <br>
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class HttpRequest extends Request<HttpContext> {

    public static final String SESSIONID_NAME = "JSESSIONID";

    @Comment("Method GET/POST/...")
    private String method;

    private String protocol;

    protected String requestURI;

    private long contentLength = -1;

    private String contentType;

    private String host;

    private String connection;

    @Comment("原始的cookie字符串，解析后值赋给HttpCookie[] cookies")
    protected String cookie;

    private HttpCookie[] cookies;

    protected String newsessionid;

    protected final DefaultAnyValue header = new DefaultAnyValue();

    protected final DefaultAnyValue params = new DefaultAnyValue();

    private final ByteArray array = new ByteArray();

    private boolean bodyparsed = false;

    protected boolean boundary = false;

    protected int moduleid;

    protected int actionid;

    protected Object currentUser;

    private final String remoteAddrHeader;

    Object attachment; //仅供HttpServlet传递Entry使用

    public HttpRequest(HttpContext context, String remoteAddrHeader) {
        super(context);
        this.remoteAddrHeader = remoteAddrHeader;
    }

    protected boolean isWebSocket() {
        return connection != null && connection.contains("Upgrade") && "GET".equalsIgnoreCase(method) && "websocket".equalsIgnoreCase(getHeader("Upgrade"));
    }

    protected void setKeepAlive(boolean keepAlive) {
        this.keepAlive = keepAlive;
    }

    protected boolean isKeepAlive() {
        return this.keepAlive;
    }

    protected AsyncConnection getChannel() {
        return this.channel;
    }

    protected JsonConvert getJsonConvert() {
        return this.jsonConvert;
    }

    @Override
    protected int readHeader(final ByteBuffer buffer) {
        if (!readLine(buffer, array)) return -1;
        Charset charset = this.context.getCharset();
        int index = 0;
        int offset = array.find(index, ' ');
        if (offset <= 0) return -1;
        this.method = array.toString(index, offset, charset).trim();
        index = ++offset;
        offset = array.find(index, ' ');
        if (offset <= 0) return -1;
        int off = array.find(index, '#');
        if (off > 0) offset = off;
        int qst = array.find(index, offset, (byte) '?');
        if (qst > 0) {
            this.requestURI = array.toDecodeString(index, qst - index, charset).trim();
            addParameter(array, qst + 1, offset - qst - 1);
        } else {
            this.requestURI = array.toDecodeString(index, offset - index, charset).trim();
        }
        index = ++offset;
        this.protocol = array.toString(index, array.size() - index, charset).trim();
        while (readLine(buffer, array)) {
            if (array.size() < 2) break;
            index = 0;
            offset = array.find(index, ':');
            if (offset <= 0) return -1;
            String name = array.toString(index, offset, charset).trim();
            index = offset + 1;
            String value = array.toString(index, array.size() - index, charset).trim();
            switch (name) {
                case "Content-Type":
                case "content-type":
                    this.contentType = value;
                    break;
                case "Content-Length":
                case "content-length":
                    this.contentLength = Long.decode(value);
                    break;
                case "Host":
                case "host":
                    this.host = value;
                    break;
                case "Cookie":
                case "cookie":
                    if (this.cookie == null || this.cookie.isEmpty()) {
                        this.cookie = value;
                    } else {
                        this.cookie += ";" + value;
                    }
                    break;
                case "Connection":
                case "connection":
                    this.connection = value;
                    if (context.getAliveTimeoutSeconds() >= 0) {
                        this.setKeepAlive(!"close".equalsIgnoreCase(value));
                    }
                    break;
                case "user-agent":
                    header.addValue("User-Agent", value);
                    break;
                default:
                    header.addValue(name, value);
            }
        }
        array.clear();
        if (buffer.hasRemaining()) array.write(buffer, buffer.remaining());
        if (this.contentType != null && this.contentType.contains("boundary=")) {
            this.boundary = true;
        }
        if (this.boundary) this.keepAlive = false; //文件上传必须设置keepAlive为false，因为文件过大时用户不一定会skip掉多余的数据
        if (this.contentLength > 0 && (this.contentType == null || !this.boundary)) {
            if (this.contentLength > context.getMaxbody()) return -1;
            int lr = (int) this.contentLength - array.size();
            return lr > 0 ? lr : 0;
        }
        return 0;
    }

    @Override
    protected int readBody(ByteBuffer buffer) {
        int len = buffer.remaining();
        array.write(buffer, len);
        return len;
    }

    @Override
    protected void prepare() {
    }

    protected void skipBodyParse() {
        this.bodyparsed = true;
    }

    private void parseBody() {
        if (this.boundary || bodyparsed) return;
        addParameter(array, 0, array.size());
        bodyparsed = true;
    }

    private void addParameter(final ByteArray array, final int offset, final int len) {
        if (len < 1) return;
        Charset charset = this.context.getCharset();
        int limit = offset + len;
        int keypos = array.find(offset, limit, '=');
        int valpos = array.find(offset, limit, '&');
        if (keypos <= 0 || (valpos >= 0 && valpos < keypos)) {
            if (valpos > 0) addParameter(array, valpos + 1, limit - valpos - 1);
            return;
        }
        String name = array.toDecodeString(offset, keypos - offset, charset);
        if (name.charAt(0) == '<') return; //内容可能是xml格式; 如: <?xml version="1.0"
        ++keypos;
        String value = array.toDecodeString(keypos, (valpos < 0) ? (limit - keypos) : (valpos - keypos), charset);
        this.params.addValue(name, value);
        if (valpos >= 0) {
            addParameter(array, valpos + 1, limit - valpos - 1);
        }
    }

    private boolean readLine(ByteBuffer buffer, ByteArray bytes) {
        byte lasted = '\r';
        bytes.clear();
        for (;;) {
            if (!buffer.hasRemaining()) {
                if (lasted != '\r') bytes.write(lasted);
                return false;
            }
            byte b = buffer.get();
            if (b == -1 || (lasted == '\r' && b == '\n')) break;
            if (lasted != '\r') bytes.write(lasted);
            lasted = b;
        }
        return true;
    }

    @Override
    protected <T> T setProperty(String name, T value) {
        return super.setProperty(name, value);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected <T> T getProperty(String name) {
        return super.getProperty(name);
    }

    @Override
    protected <T> T removeProperty(String name) {
        return super.removeProperty(name);
    }

    /**
     * 设置当前用户信息, 通常在HttpServlet.preExecute方法里设置currentUser <br>
     * 数据类型由&#64;HttpUserType指定
     *
     * @param <T>  泛型
     * @param user 用户信息
     *
     * @return HttpRequest
     */
    public <T> HttpRequest setCurrentUser(T user) {
        this.currentUser = user;
        return this;
    }

    /**
     * 获取当前用户信息<br>
     * 数据类型由&#64;HttpUserType指定
     *
     * @param <T> &#64;HttpUserType指定的用户信息类型
     *
     * @return 用户信息
     */
    @SuppressWarnings("unchecked")
    public <T> T currentUser() {
        return (T) this.currentUser;
    }

    /**
     * 获取模块ID，来自&#64;HttpServlet.moduleid()
     *
     * @return 模块ID
     */
    public int getModuleid() {
        return this.moduleid;
    }

    /**
     * 获取操作ID，来自&#64;HttpMapping.actionid()
     *
     * @return 模块ID
     */
    public int getActionid() {
        return this.actionid;
    }

    /**
     * 获取客户端地址IP
     *
     * @return 地址
     */
    public SocketAddress getRemoteAddress() {
        return this.channel == null || !this.channel.isOpen() ? null : this.channel.getRemoteAddress();
    }

    /**
     * 获取客户端地址IP, 与getRemoteAddress() 的区别在于：本方法优先取header中指定为RemoteAddress名的值，没有则返回getRemoteAddress()的getHostAddress()。<br>
     * 本方法适用于服务前端有如nginx的代理服务器进行中转，通过 getRemoteAddress()是获取不到客户端的真实IP。
     *
     * @return 地址
     */
    public String getRemoteAddr() {
        if (remoteAddrHeader != null) {
            String val = getHeader(remoteAddrHeader);
            if (val != null) return val;
        }
        SocketAddress addr = getRemoteAddress();
        if (addr == null) return "";
        if (addr instanceof InetSocketAddress) return ((InetSocketAddress) addr).getAddress().getHostAddress();
        return String.valueOf(addr);
    }

    /**
     * 获取请求内容指定的编码字符串
     *
     * @param charset 编码
     *
     * @return 内容
     */
    public String getBody(final Charset charset) {
        return charset == null ? array.toString() : array.toString(charset);
    }

    /**
     * 获取请求内容的UTF-8编码字符串
     *
     * @return 内容
     */
    public String getBodyUTF8() {
        return array.toString(StandardCharsets.UTF_8);
    }

    /**
     * 获取请求内容的JavaBean对象
     *
     * @param <T>  泛型
     * @param type 类型
     *
     * @return 内容
     */
    public <T> T getBodyJson(java.lang.reflect.Type type) {
        String str = array.toString(StandardCharsets.UTF_8);
        if (str == null || str.isEmpty()) return null;
        return context.getJsonConvert().convertFrom(type, str);
    }

    /**
     * 获取请求内容的JavaBean对象
     *
     * @param <T>     泛型
     * @param convert JsonConvert
     * @param type    类型
     *
     * @return 内容
     */
    public <T> T getBodyJson(JsonConvert convert, java.lang.reflect.Type type) {
        String str = array.toString(StandardCharsets.UTF_8);
        if (str == null || str.isEmpty()) return null;
        return convert.convertFrom(type, str);
    }

    /**
     * 获取请求内容的byte[]
     *
     * @return 内容
     */
    public byte[] getBody() {
        return array.getBytes();
    }

    /**
     * 直接获取body对象
     *
     * @return body对象
     */
    protected ByteArray getDirectBody() {
        return array;
    }

    @Override
    public String toString() {
        parseBody();
        return this.getClass().getSimpleName() + "{\r\n    method: " + this.method + ", \r\n    requestURI: " + this.requestURI
            + ", \r\n    remoteAddr: " + this.getRemoteAddr() + ", \r\n    cookies: " + this.cookie + ", \r\n    contentType: " + this.contentType
            + ", \r\n    connection: " + this.connection + ", \r\n    protocol: " + this.protocol + ", \r\n    host: " + this.host
            + ", \r\n    contentLength: " + this.contentLength + ", \r\n    bodyLength: " + this.array.size() + (this.boundary || this.array.isEmpty() ? "" : (", \r\n    bodyContent: " + this.getBodyUTF8()))
            + ", \r\n    params: " + this.params.toString(4) + ", \r\n    header: " + this.header.toString(4) + "\r\n}";
    }

    /**
     * 获取文件上传对象
     *
     * @return 文件上传对象
     */
    public final MultiContext getMultiContext() {
        return new MultiContext(context.getCharset(), this.getContentType(), this.params,
            new BufferedInputStream(Channels.newInputStream(this.channel), Math.max(array.size(), 8192)) {
            {
                array.copyTo(this.buf);
                this.count = array.size();
            }
        }, null);
    }

    /**
     * 获取文件上传信息列表
     *
     * @return 文件上传对象集合
     *
     * @throws IOException IO异常
     */
    public final Iterable<MultiPart> multiParts() throws IOException {
        return getMultiContext().parts();
    }

    @Override
    protected void recycle() {
        this.cookie = null;
        this.cookies = null;
        this.newsessionid = null;
        this.method = null;
        this.protocol = null;
        this.requestURI = null;
        this.contentType = null;
        this.host = null;
        this.connection = null;
        this.contentLength = -1;
        this.boundary = false;
        this.bodyparsed = false;
        this.moduleid = 0;
        this.actionid = 0;
        this.currentUser = null;

        this.attachment = null;

        this.header.clear();
        this.params.clear();
        this.array.clear();
        super.recycle();
    }

    /**
     * 获取sessionid
     *
     * @param create 无sessionid是否自动创建
     *
     * @return sessionid
     */
    public String getSessionid(boolean create) {
        String sessionid = getCookie(SESSIONID_NAME, null);
        if (create && (sessionid == null || sessionid.isEmpty())) {
            sessionid = context.createSessionid();
            this.newsessionid = sessionid;
        }
        return sessionid;
    }

    /**
     * 更新sessionid
     *
     * @return 新的sessionid值
     */
    public String changeSessionid() {
        this.newsessionid = context.createSessionid();
        return newsessionid;
    }

    /**
     * 指定值更新sessionid
     *
     * @param newsessionid 新sessionid值
     *
     * @return 新的sessionid值
     */
    public String changeSessionid(String newsessionid) {
        this.newsessionid = newsessionid == null ? context.createSessionid() : newsessionid.trim();
        return newsessionid;
    }

    /**
     * 使sessionid失效
     */
    public void invalidateSession() {
        this.newsessionid = ""; //为空表示删除sessionid
    }

    /**
     * 获取所有Cookie对象
     *
     * @return cookie对象数组
     */
    public HttpCookie[] getCookies() {
        if (this.cookies == null) this.cookies = parseCookies(this.cookie);
        return this.cookies;
    }

    /**
     * 获取Cookie值
     *
     * @param name cookie名
     *
     * @return cookie值
     */
    public String getCookie(String name) {
        return getCookie(name, null);
    }

    /**
     * 获取Cookie值， 没有返回默认值
     *
     * @param name    cookie名
     * @param dfvalue 默认cookie值
     *
     * @return cookie值
     */
    public String getCookie(String name, String dfvalue) {
        for (HttpCookie c : getCookies()) {
            if (name.equals(c.getName())) return c.getValue();
        }
        return dfvalue;
    }

    private static HttpCookie[] parseCookies(String cookiestr) {
        if (cookiestr == null || cookiestr.isEmpty()) return new HttpCookie[0];
        String str = cookiestr.replaceAll("(^;)|(;$)", "").replaceAll(";+", ";");
        if (str.isEmpty()) return new HttpCookie[0];
        String[] strs = str.split(";");
        HttpCookie[] cookies = new HttpCookie[strs.length];
        for (int i = 0; i < strs.length; i++) {
            String s = strs[i];
            int pos = s.indexOf('=');
            String v = (pos < 0 ? "" : s.substring(pos + 1));
            if (v.indexOf('"') == 0 && v.lastIndexOf('"') == v.length() - 1) v = v.substring(1, v.length() - 1);
            cookies[i] = new HttpCookie((pos < 0 ? s : s.substring(0, pos)), v);
        }
        return cookies;
    }

    /**
     * 获取协议名 http、https、ws、wss等
     *
     * @return protocol
     */
    public String getProtocol() {
        return protocol;
    }

    /**
     * 获取请求方法 GET、POST等
     *
     * @return method
     */
    public String getMethod() {
        return method;
    }

    /**
     * 获取Content-Type的header值
     *
     * @return contentType
     */
    public String getContentType() {
        return contentType;
    }

    /**
     * 获取请求内容的长度, 为-1表示内容长度不确定
     *
     * @return 内容长度
     */
    public long getContentLength() {
        return contentLength;
    }

    /**
     * 获取Connection的Header值
     *
     * @return Connection
     */
    public String getConnection() {
        return connection;
    }

    /**
     * 获取Host的Header值
     *
     * @return Host
     */
    public String getHost() {
        return host;
    }

    /**
     * 获取请求的URL
     *
     * @return 请求的URL
     */
    public String getRequestURI() {
        return requestURI;
    }

    /**
     * 截取getRequestURI最后的一个/后面的部分
     *
     * @return String
     */
    public String getRequstURILastPath() {
        if (requestURI == null) return "";
        return requestURI.substring(requestURI.lastIndexOf('/') + 1);
    }

    /**
     * 获取请求URL最后的一个/后面的部分的short值   <br>
     * 例如请求URL /pipes/record/query/2   <br>
     * 获取type参数: short type = request.getRequstURILastPath((short)0); //type = 2
     *
     * @param defvalue 默认short值
     *
     * @return short值
     */
    public short getRequstURILastPath(short defvalue) {
        String val = getRequstURILastPath();
        if (val.isEmpty()) return defvalue;
        try {
            return Short.parseShort(val);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL最后的一个/后面的部分的short值   <br>
     * 例如请求URL /pipes/record/query/2   <br>
     * 获取type参数: short type = request.getRequstURILastPath(16, (short)0); //type = 2
     *
     * @param radix    进制数
     * @param defvalue 默认short值
     *
     * @return short值
     */
    public short getRequstURILastPath(int radix, short defvalue) {
        String val = getRequstURILastPath();
        if (val.isEmpty()) return defvalue;
        try {
            return Short.parseShort(val, radix);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL最后的一个/后面的部分的int值   <br>
     * 例如请求URL /pipes/record/query/2   <br>
     * 获取type参数: int type = request.getRequstURILastPath(0); //type = 2
     *
     * @param defvalue 默认int值
     *
     * @return int值
     */
    public int getRequstURILastPath(int defvalue) {
        String val = getRequstURILastPath();
        try {
            return val.isEmpty() ? defvalue : Integer.parseInt(val);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL最后的一个/后面的部分的int值   <br>
     * 例如请求URL /pipes/record/query/2   <br>
     * 获取type参数: int type = request.getRequstURILastPath(16, 0); //type = 2
     *
     * @param radix    进制数
     * @param defvalue 默认int值
     *
     * @return int值
     */
    public int getRequstURILastPath(int radix, int defvalue) {
        String val = getRequstURILastPath();
        try {
            return val.isEmpty() ? defvalue : Integer.parseInt(val, radix);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL最后的一个/后面的部分的float值   <br>
     * 例如请求URL /pipes/record/query/2   <br>
     * 获取type参数: float type = request.getRequstURILastPath(0.0f); //type = 2.0f
     *
     * @param defvalue 默认float值
     *
     * @return float值
     */
    public float getRequstURILastPath(float defvalue) {
        String val = getRequstURILastPath();
        try {
            return val.isEmpty() ? defvalue : Float.parseFloat(val);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL最后的一个/后面的部分的int值   <br>
     * 例如请求URL /pipes/record/query/2   <br>
     * 获取type参数: long type = request.getRequstURILastPath(0L); //type = 2
     *
     * @param defvalue 默认long值
     *
     * @return long值
     */
    public long getRequstURILastPath(long defvalue) {
        String val = getRequstURILastPath();
        try {
            return val.isEmpty() ? defvalue : Long.parseLong(val);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL最后的一个/后面的部分的int值   <br>
     * 例如请求URL /pipes/record/query/2   <br>
     * 获取type参数: long type = request.getRequstURILastPath(16, 0L); //type = 2
     *
     * @param radix    进制数
     * @param defvalue 默认long值
     *
     * @return long值
     */
    public long getRequstURILastPath(int radix, long defvalue) {
        String val = getRequstURILastPath();
        try {
            return val.isEmpty() ? defvalue : Long.parseLong(val, radix);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL最后的一个/后面的部分的double值   <br>
     * 例如请求URL /pipes/record/query/2   <br>
     * 获取type参数: double type = request.getRequstURILastPath(0.0); //type = 2.0
     *
     * @param defvalue 默认double值
     *
     * @return double值
     */
    public double getRequstURILastPath(double defvalue) {
        String val = getRequstURILastPath();
        try {
            return val.isEmpty() ? defvalue : Double.parseDouble(val);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     *
     * 从prefix之后截取getRequestURI再对"/"进行分隔
     * <p>
     * @param prefix 前缀
     *
     * @return String[]
     */
    public String[] getRequstURIPaths(String prefix) {
        if (requestURI == null || prefix == null) return new String[0];
        return requestURI.substring(requestURI.indexOf(prefix) + prefix.length() + (prefix.endsWith("/") ? 0 : 1)).split("/");
    }

    /**
     * 获取请求URL分段中含prefix段的值   <br>
     * 例如请求URL /pipes/record/query/name:hello   <br>
     * 获取name参数: String name = request.getRequstURIPath("name:", "none");
     *
     * @param prefix   prefix段前缀
     * @param defvalue 默认值
     *
     * @return prefix截断后的值
     */
    public String getRequstURIPath(String prefix, String defvalue) {
        if (requestURI == null || prefix == null || prefix.isEmpty()) return defvalue;
        int pos = requestURI.indexOf(prefix);
        if (pos < 0) return defvalue;
        String sub = requestURI.substring(pos + prefix.length());
        pos = sub.indexOf('/');
        return pos < 0 ? sub : sub.substring(0, pos);
    }

    /**
     * 获取请求URL分段中含prefix段的short值   <br>
     * 例如请求URL /pipes/record/query/type:10   <br>
     * 获取type参数: short type = request.getRequstURIPath("type:", (short)0);
     *
     * @param prefix   prefix段前缀
     * @param defvalue 默认short值
     *
     * @return short值
     */
    public short getRequstURIPath(String prefix, short defvalue) {
        String val = getRequstURIPath(prefix, null);
        try {
            return val == null ? defvalue : Short.parseShort(val);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL分段中含prefix段的short值   <br>
     * 例如请求URL /pipes/record/query/type:a   <br>
     * 获取type参数: short type = request.getRequstURIPath(16, "type:", (short)0); //type = 10
     *
     * @param radix    进制数
     * @param prefix   prefix段前缀
     * @param defvalue 默认short值
     *
     * @return short值
     */
    public short getRequstURIPath(int radix, String prefix, short defvalue) {
        String val = getRequstURIPath(prefix, null);
        try {
            return val == null ? defvalue : Short.parseShort(val, radix);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL分段中含prefix段的int值  <br>
     * 例如请求URL /pipes/record/query/offset:0/limit:50   <br>
     * 获取offset参数: int offset = request.getRequstURIPath("offset:", 0);   <br>
     * 获取limit参数: int limit = request.getRequstURIPath("limit:", 20);  <br>
     *
     * @param prefix   prefix段前缀
     * @param defvalue 默认int值
     *
     * @return int值
     */
    public int getRequstURIPath(String prefix, int defvalue) {
        String val = getRequstURIPath(prefix, null);
        try {
            return val == null ? defvalue : Integer.parseInt(val);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL分段中含prefix段的int值  <br>
     * 例如请求URL /pipes/record/query/offset:0/limit:50   <br>
     * 获取offset参数: int offset = request.getRequstURIPath("offset:", 0);   <br>
     * 获取limit参数: int limit = request.getRequstURIPath(16, "limit:", 20); // limit = 16  <br>
     *
     * @param radix    进制数
     * @param prefix   prefix段前缀
     * @param defvalue 默认int值
     *
     * @return int值
     */
    public int getRequstURIPath(int radix, String prefix, int defvalue) {
        String val = getRequstURIPath(prefix, null);
        try {
            return val == null ? defvalue : Integer.parseInt(val, radix);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL分段中含prefix段的float值   <br>
     * 例如请求URL /pipes/record/query/point:40.0   <br>
     * 获取time参数: float point = request.getRequstURIPath("point:", 0.0f);
     *
     * @param prefix   prefix段前缀
     * @param defvalue 默认float值
     *
     * @return float值
     */
    public float getRequstURIPath(String prefix, float defvalue) {
        String val = getRequstURIPath(prefix, null);
        try {
            return val == null ? defvalue : Float.parseFloat(val);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL分段中含prefix段的long值   <br>
     * 例如请求URL /pipes/record/query/time:1453104341363/id:40   <br>
     * 获取time参数: long time = request.getRequstURIPath("time:", 0L);
     *
     * @param prefix   prefix段前缀
     * @param defvalue 默认long值
     *
     * @return long值
     */
    public long getRequstURIPath(String prefix, long defvalue) {
        String val = getRequstURIPath(prefix, null);
        try {
            return val == null ? defvalue : Long.parseLong(val);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL分段中含prefix段的long值   <br>
     * 例如请求URL /pipes/record/query/time:1453104341363/id:40   <br>
     * 获取time参数: long time = request.getRequstURIPath(16, "time:", 0L);
     *
     * @param radix    进制数
     * @param prefix   prefix段前缀
     * @param defvalue 默认long值
     *
     * @return long值
     */
    public long getRequstURIPath(int radix, String prefix, long defvalue) {
        String val = getRequstURIPath(prefix, null);
        try {
            return val == null ? defvalue : Long.parseLong(val, radix);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    /**
     * 获取请求URL分段中含prefix段的double值   <br>
     * 例如请求URL /pipes/record/query/point:40.0   <br>
     * 获取time参数: double point = request.getRequstURIPath("point:", 0.0);
     *
     * @param prefix   prefix段前缀
     * @param defvalue 默认double值
     *
     * @return double值
     */
    public double getRequstURIPath(String prefix, double defvalue) {
        String val = getRequstURIPath(prefix, null);
        try {
            return val == null ? defvalue : Double.parseDouble(val);
        } catch (NumberFormatException e) {
            return defvalue;
        }
    }

    //------------------------------------------------------------------------------
    /**
     * 获取请求Header总对象
     *
     * @return AnyValue
     */
    public AnyValue getHeaders() {
        return header;
    }

    /**
     * 将请求Header转换成Map
     *
     * @param map Map
     *
     * @return Map
     */
    public Map<String, String> getHeadersToMap(Map<String, String> map) {
        if (map == null) map = new LinkedHashMap<>();
        final Map<String, String> map0 = map;
        header.forEach((k, v) -> map0.put(k, v));
        return map0;
    }

    /**
     * 获取所有的header名
     *
     * @return header名数组
     */
    public String[] getHeaderNames() {
        return header.getNames();
    }

    /**
     * 获取指定的header值
     *
     * @param name header名
     *
     * @return header值
     */
    public String getHeader(String name) {
        return header.getValue(name);
    }

    /**
     * 获取指定的header值, 没有返回默认值
     *
     * @param name         header名
     * @param defaultValue 默认值
     *
     * @return header值
     */
    public String getHeader(String name, String defaultValue) {
        return header.getValue(name, defaultValue);
    }

    /**
     * 获取指定的header的json值
     *
     * @param <T>  泛型
     * @param type 反序列化的类名
     * @param name header名
     *
     * @return header值
     */
    public <T> T getJsonHeader(java.lang.reflect.Type type, String name) {
        String v = getHeader(name);
        return v == null || v.isEmpty() ? null : jsonConvert.convertFrom(type, v);
    }

    /**
     * 获取指定的header的json值
     *
     * @param <T>     泛型
     * @param convert JsonConvert对象
     * @param type    反序列化的类名
     * @param name    header名
     *
     * @return header值
     */
    public <T> T getJsonHeader(JsonConvert convert, java.lang.reflect.Type type, String name) {
        String v = getHeader(name);
        return v == null || v.isEmpty() ? null : convert.convertFrom(type, v);
    }

    /**
     * 获取指定的header的boolean值, 没有返回默认boolean值
     *
     * @param name         header名
     * @param defaultValue 默认boolean值
     *
     * @return header值
     */
    public boolean getBooleanHeader(String name, boolean defaultValue) {
        return header.getBoolValue(name, defaultValue);
    }

    /**
     * 获取指定的header的short值, 没有返回默认short值
     *
     * @param name         header名
     * @param defaultValue 默认short值
     *
     * @return header值
     */
    public short getShortHeader(String name, short defaultValue) {
        return header.getShortValue(name, defaultValue);
    }

    /**
     * 获取指定的header的short值, 没有返回默认short值
     *
     * @param radix        进制数
     * @param name         header名
     * @param defaultValue 默认short值
     *
     * @return header值
     */
    public short getShortHeader(int radix, String name, short defaultValue) {
        return header.getShortValue(name, defaultValue);
    }

    /**
     * 获取指定的header的short值, 没有返回默认short值
     *
     * @param name         header名
     * @param defaultValue 默认short值
     *
     * @return header值
     */
    public short getShortHeader(String name, int defaultValue) {
        return header.getShortValue(name, (short) defaultValue);
    }

    /**
     * 获取指定的header的short值, 没有返回默认short值
     *
     * @param radix        进制数
     * @param name         header名
     * @param defaultValue 默认short值
     *
     * @return header值
     */
    public short getShortHeader(int radix, String name, int defaultValue) {
        return header.getShortValue(radix, name, (short) defaultValue);
    }

    /**
     * 获取指定的header的int值, 没有返回默认int值
     *
     * @param name         header名
     * @param defaultValue 默认int值
     *
     * @return header值
     */
    public int getIntHeader(String name, int defaultValue) {
        return header.getIntValue(name, defaultValue);
    }

    /**
     * 获取指定的header的int值, 没有返回默认int值
     *
     * @param radix        进制数
     * @param name         header名
     * @param defaultValue 默认int值
     *
     * @return header值
     */
    public int getIntHeader(int radix, String name, int defaultValue) {
        return header.getIntValue(radix, name, defaultValue);
    }

    /**
     * 获取指定的header的long值, 没有返回默认long值
     *
     * @param name         header名
     * @param defaultValue 默认long值
     *
     * @return header值
     */
    public long getLongHeader(String name, long defaultValue) {
        return header.getLongValue(name, defaultValue);
    }

    /**
     * 获取指定的header的long值, 没有返回默认long值
     *
     * @param radix        进制数
     * @param name         header名
     * @param defaultValue 默认long值
     *
     * @return header值
     */
    public long getLongHeader(int radix, String name, long defaultValue) {
        return header.getLongValue(radix, name, defaultValue);
    }

    /**
     * 获取指定的header的float值, 没有返回默认float值
     *
     * @param name         header名
     * @param defaultValue 默认float值
     *
     * @return header值
     */
    public float getFloatHeader(String name, float defaultValue) {
        return header.getFloatValue(name, defaultValue);
    }

    /**
     * 获取指定的header的double值, 没有返回默认double值
     *
     * @param name         header名
     * @param defaultValue 默认double值
     *
     * @return header值
     */
    public double getDoubleHeader(String name, double defaultValue) {
        return header.getDoubleValue(name, defaultValue);
    }

    //------------------------------------------------------------------------------
    /**
     * 获取请求参数总对象
     *
     * @return AnyValue
     */
    public AnyValue getParameters() {
        parseBody();
        return params;
    }

    /**
     * 将请求参数转换成Map
     *
     * @param map Map
     *
     * @return Map
     */
    public Map<String, String> getParametersToMap(Map<String, String> map) {
        if (map == null) map = new LinkedHashMap<>();
        final Map<String, String> map0 = map;
        getParameters().forEach((k, v) -> map0.put(k, v));
        return map0;
    }

    /**
     * 将请求参数转换成String, 字符串格式为: bean1={}&amp;id=13&amp;name=xxx <br>
     * 不会返回null，没有参数返回空字符串
     *
     *
     * @return String
     */
    public String getParametersToString() {
        return getParametersToString(null);
    }

    /**
     * 将请求参数转换成String, 字符串格式为: bean1={}&amp;id=13&amp;name=xxx <br>
     * 不会返回null，没有参数返回空字符串
     *
     * @param prefix 拼接前缀， 如果无参数，返回的字符串不会含有拼接前缀
     *
     * @return String
     */
    public String getParametersToString(String prefix) {
        final StringBuilder sb = new StringBuilder();
        getParameters().forEach((k, v) -> {
            if (sb.length() > 0) sb.append('&');
            try {
                sb.append(k).append('=').append(URLEncoder.encode(v, "UTF-8"));
            } catch (IOException ex) {
                throw new RuntimeException(ex);
            }
        });
        return (sb.length() > 0 && prefix != null) ? (prefix + sb) : sb.toString();
    }

    /**
     * 获取所有参数名
     *
     * @return 参数名数组
     */
    public String[] getParameterNames() {
        parseBody();
        return params.getNames();
    }

    /**
     * 获取指定的参数值
     *
     * @param name 参数名
     *
     * @return 参数值
     */
    public String getParameter(String name) {
        parseBody();
        return params.getValue(name);
    }

    /**
     * 获取指定的参数值, 没有返回默认值
     *
     * @param name         参数名
     * @param defaultValue 默认值
     *
     * @return 参数值
     */
    public String getParameter(String name, String defaultValue) {
        parseBody();
        return params.getValue(name, defaultValue);
    }

    /**
     * 获取指定的参数json值
     *
     * @param <T>  泛型
     * @param type 反序列化的类名
     * @param name 参数名
     *
     * @return 参数值
     */
    public <T> T getJsonParameter(java.lang.reflect.Type type, String name) {
        String v = getParameter(name);
        return v == null || v.isEmpty() ? null : jsonConvert.convertFrom(type, v);
    }

    /**
     * 获取指定的参数json值
     *
     * @param <T>     泛型
     * @param convert JsonConvert对象
     * @param type    反序列化的类名
     * @param name    参数名
     *
     * @return 参数值
     */
    public <T> T getJsonParameter(JsonConvert convert, java.lang.reflect.Type type, String name) {
        String v = getParameter(name);
        return v == null || v.isEmpty() ? null : convert.convertFrom(type, v);
    }

    /**
     * 获取指定的参数boolean值, 没有返回默认boolean值
     *
     * @param name         参数名
     * @param defaultValue 默认boolean值
     *
     * @return 参数值
     */
    public boolean getBooleanParameter(String name, boolean defaultValue) {
        parseBody();
        return params.getBoolValue(name, defaultValue);
    }

    /**
     * 获取指定的参数short值, 没有返回默认short值
     *
     * @param name         参数名
     * @param defaultValue 默认short值
     *
     * @return 参数值
     */
    public short getShortParameter(String name, short defaultValue) {
        parseBody();
        return params.getShortValue(name, defaultValue);
    }

    /**
     * 获取指定的参数short值, 没有返回默认short值
     *
     * @param radix        进制数
     * @param name         参数名
     * @param defaultValue 默认short值
     *
     * @return 参数值
     */
    public short getShortParameter(int radix, String name, short defaultValue) {
        parseBody();
        return params.getShortValue(radix, name, defaultValue);
    }

    /**
     * 获取指定的参数short值, 没有返回默认short值
     *
     * @param name         参数名
     * @param defaultValue 默认short值
     *
     * @return 参数值
     */
    public short getShortParameter(String name, int defaultValue) {
        parseBody();
        return params.getShortValue(name, (short) defaultValue);
    }

    /**
     * 获取指定的参数int值, 没有返回默认int值
     *
     * @param name         参数名
     * @param defaultValue 默认int值
     *
     * @return 参数值
     */
    public int getIntParameter(String name, int defaultValue) {
        parseBody();
        return params.getIntValue(name, defaultValue);
    }

    /**
     * 获取指定的参数int值, 没有返回默认int值
     *
     * @param radix        进制数
     * @param name         参数名
     * @param defaultValue 默认int值
     *
     * @return 参数值
     */
    public int getIntParameter(int radix, String name, int defaultValue) {
        parseBody();
        return params.getIntValue(radix, name, defaultValue);
    }

    /**
     * 获取指定的参数long值, 没有返回默认long值
     *
     * @param name         参数名
     * @param defaultValue 默认long值
     *
     * @return 参数值
     */
    public long getLongParameter(String name, long defaultValue) {
        parseBody();
        return params.getLongValue(name, defaultValue);
    }

    /**
     * 获取指定的参数long值, 没有返回默认long值
     *
     * @param radix        进制数
     * @param name         参数名
     * @param defaultValue 默认long值
     *
     * @return 参数值
     */
    public long getLongParameter(int radix, String name, long defaultValue) {
        parseBody();
        return params.getLongValue(radix, name, defaultValue);
    }

    /**
     * 获取指定的参数float值, 没有返回默认float值
     *
     * @param name         参数名
     * @param defaultValue 默认float值
     *
     * @return 参数值
     */
    public float getFloatParameter(String name, float defaultValue) {
        parseBody();
        return params.getFloatValue(name, defaultValue);
    }

    /**
     * 获取指定的参数double值, 没有返回默认double值
     *
     * @param name         参数名
     * @param defaultValue 默认double值
     *
     * @return 参数值
     */
    public double getDoubleParameter(String name, double defaultValue) {
        parseBody();
        return params.getDoubleValue(name, defaultValue);
    }

    /**
     * 获取翻页对象 同 getFlipper("flipper", false, 0);
     *
     * @return Flipper翻页对象
     */
    public org.redkale.source.Flipper getFlipper() {
        return getFlipper(false, 0);
    }

    /**
     * 获取翻页对象 同 getFlipper("flipper", needcreate, 0);
     *
     * @param needcreate 无参数时是否创建新Flipper对象
     *
     * @return Flipper翻页对象
     */
    public org.redkale.source.Flipper getFlipper(boolean needcreate) {
        return getFlipper(needcreate, 0);
    }

    /**
     * 获取翻页对象 同 getFlipper("flipper", false, maxLimit);
     *
     * @param maxLimit 最大行数， 小于1则值为Flipper.DEFAULT_LIMIT
     *
     * @return Flipper翻页对象
     */
    public org.redkale.source.Flipper getFlipper(int maxLimit) {
        return getFlipper(false, maxLimit);
    }

    /**
     * 获取翻页对象 同 getFlipper("flipper", needcreate, maxLimit)
     *
     * @param needcreate 无参数时是否创建新Flipper对象
     * @param maxLimit   最大行数， 小于1则值为Flipper.DEFAULT_LIMIT
     *
     * @return Flipper翻页对象
     */
    public org.redkale.source.Flipper getFlipper(boolean needcreate, int maxLimit) {
        return getFlipper("flipper", needcreate, maxLimit);
    }

    /**
     * 获取翻页对象 https://redkale.org/pipes/records/list/offset:0/limit:20/sort:createtime%20ASC  <br>
     * https://redkale.org/pipes/records/list?flipper={'offset':0,'limit':20, 'sort':'createtime ASC'}  <br>
     * 以上两种接口都可以获取到翻页对象
     *
     *
     * @param name       Flipper对象的参数名，默认为 "flipper"
     * @param needcreate 无参数时是否创建新Flipper对象
     * @param maxLimit   最大行数， 小于1则值为Flipper.DEFAULT_LIMIT
     *
     * @return Flipper翻页对象
     */
    public org.redkale.source.Flipper getFlipper(String name, boolean needcreate, int maxLimit) {
        org.redkale.source.Flipper flipper = getJsonParameter(org.redkale.source.Flipper.class, name);
        if (flipper == null) {
            if (maxLimit < 1) maxLimit = org.redkale.source.Flipper.DEFAULT_LIMIT;
            int limit = getRequstURIPath("limit:", maxLimit);
            int offset = getRequstURIPath("offset:", 0);
            String sort = getRequstURIPath("sort:", "");
            if (limit > 0) flipper = new org.redkale.source.Flipper(limit, offset, sort);
        } else if (flipper.getLimit() < 1 || (maxLimit > 0 && flipper.getLimit() > maxLimit)) {
            flipper.setLimit(maxLimit);
        }
        if (flipper != null || !needcreate) return flipper;
        if (maxLimit < 1) maxLimit = org.redkale.source.Flipper.DEFAULT_LIMIT;
        return new org.redkale.source.Flipper(maxLimit);
    }
}
