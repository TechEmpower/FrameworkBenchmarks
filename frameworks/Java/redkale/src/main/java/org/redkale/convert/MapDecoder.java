/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import org.redkale.util.Creator;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Map;

/**
 * Map的反序列化操作类 <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <K> Map key的数据类型
 * @param <V> Map value的数据类型
 */
@SuppressWarnings("unchecked")
public final class MapDecoder<K, V> implements Decodeable<Reader, Map<K, V>> {

    private final Type type;

    private final Type keyType;

    private final Type valueType;

    protected Creator<Map<K, V>> creator;

    protected final Decodeable<Reader, K> keyDecoder;

    protected final Decodeable<Reader, V> valueDecoder;

    private boolean inited = false;

    private final Object lock = new Object();

    public MapDecoder(final ConvertFactory factory, final Type type) {
        this.type = type;
        try {
            if (type instanceof ParameterizedType) {
                final ParameterizedType pt = (ParameterizedType) type;
                this.keyType = pt.getActualTypeArguments()[0];
                this.valueType = pt.getActualTypeArguments()[1];
                this.creator = factory.loadCreator((Class) pt.getRawType());
                factory.register(type, this);
                this.keyDecoder = factory.loadDecoder(this.keyType);
                this.valueDecoder = factory.loadDecoder(this.valueType);
            } else if (factory.isReversible()) {
                this.keyType = Object.class;
                this.valueType = Object.class;
                this.creator = factory.loadCreator((Class) type);
                this.keyDecoder = factory.loadDecoder(this.keyType);
                this.valueDecoder = factory.loadDecoder(this.valueType);
            } else {
                throw new ConvertException("mapdecoder not support the type (" + type + ")");
            }
        } finally {
            inited = true;
            synchronized (lock) {
                lock.notifyAll();
            }
        }
    }

    @Override
    public Map<K, V> convertFrom(Reader in) {
        if (this.keyDecoder == null || this.valueDecoder == null) {
            if (!this.inited) {
                synchronized (lock) {
                    try {
                        lock.wait();
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        }
        final int len = in.readMapB();
        if (len == Reader.SIGN_NULL) return null;
        final Map<K, V> result = this.creator.create();
        if (len == Reader.SIGN_NOLENGTH) {
            while (in.hasNext()) {
                K key = keyDecoder.convertFrom(in);
                in.readBlank();
                V value = valueDecoder.convertFrom(in);
                result.put(key, value);
            }
        } else {
            for (int i = 0; i < len; i++) {
                K key = keyDecoder.convertFrom(in);
                in.readBlank();
                V value = valueDecoder.convertFrom(in);
                result.put(key, value);
            }
        }
        in.readMapE();
        return result;
    }

    @Override
    public Type getType() {
        return this.type;
    }

}
