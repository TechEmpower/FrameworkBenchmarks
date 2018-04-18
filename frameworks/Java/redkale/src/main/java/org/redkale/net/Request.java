/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net;

import java.nio.ByteBuffer;
import java.util.*;
import org.redkale.convert.bson.BsonConvert;
import org.redkale.convert.json.JsonConvert;

/**
 * 协议请求对象
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <C> Context子类型
 */
public abstract class Request<C extends Context> {

    protected final C context;

    protected final BsonConvert bsonConvert;

    protected final JsonConvert jsonConvert;

    protected long createtime;

    protected boolean keepAlive;

    protected AsyncConnection channel;

    protected ByteBuffer readBuffer;

    /**
     * properties 与 attributes 的区别在于：调用recycle时， attributes会被清空而properties会保留;
     * properties 通常存放需要永久绑定在request里的一些对象
     */
    private final Map<String, Object> properties = new HashMap<>();

    protected final Map<String, Object> attributes = new HashMap<>();

    protected Request(C context) {
        this.context = context;
        this.readBuffer = context.pollBuffer();
        this.bsonConvert = context.getBsonConvert();
        this.jsonConvert = context.getJsonConvert();
    }

    protected ByteBuffer pollReadBuffer() {
        ByteBuffer buffer = this.readBuffer;
        this.readBuffer = null;
        if (buffer == null) buffer = context.pollBuffer();
        return buffer;
    }

    protected void offerReadBuffer(ByteBuffer buffer) {
        if (buffer == null) return;
        if (this.readBuffer == null) {
            buffer.clear();
            this.readBuffer = buffer;
        } else {
            context.offerBuffer(buffer);
        }
    }

    /**
     * 返回值：Integer.MIN_VALUE: 帧数据； -1：数据不合法； 0：解析完毕； &gt;0: 需再读取的字节数。
     *
     * @param buffer ByteBuffer对象
     *
     * @return 缺少的字节数
     */
    protected abstract int readHeader(ByteBuffer buffer);

    /**
     * 读取buffer，并返回读取的有效数据长度
     *
     * @param buffer ByteBuffer对象
     *
     * @return 有效数据长度
     */
    protected abstract int readBody(ByteBuffer buffer);

    protected abstract void prepare();

    protected void recycle() {
        createtime = 0;
        keepAlive = false;
        attributes.clear();
        channel = null; // close it by response
    }

    protected <T> T setProperty(String name, T value) {
        properties.put(name, value);
        return value;
    }

    @SuppressWarnings("unchecked")
    protected <T> T getProperty(String name) {
        return (T) properties.get(name);
    }

    @SuppressWarnings("unchecked")
    protected <T> T removeProperty(String name) {
        return (T) properties.remove(name);
    }

    protected Map<String, Object> getProperties() {
        return properties;
    }

    public <T> T setAttribute(String name, T value) {
        attributes.put(name, value);
        return value;
    }

    @SuppressWarnings("unchecked")
    public <T> T getAttribute(String name) {
        return (T) attributes.get(name);
    }

    @SuppressWarnings("unchecked")
    public <T> T removeAttribute(String name) {
        return (T) attributes.remove(name);
    }

    public Map<String, Object> getAttributes() {
        return attributes;
    }

    public C getContext() {
        return this.context;
    }

    public long getCreatetime() {
        return createtime;
    }

}
