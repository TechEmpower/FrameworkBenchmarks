/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.reflect.Type;
import java.util.concurrent.CompletableFuture;

/**
 * 对不明类型的对象进行序列化； BSON序列化时将对象的类名写入Writer，JSON则不写入。
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> 序列化的泛型类型
 */
public final class AnyEncoder<T> implements Encodeable<Writer, T> {

    final ConvertFactory factory;

    AnyEncoder(ConvertFactory factory) {
        this.factory = factory;
    }

    @Override
    @SuppressWarnings("unchecked")
    public void convertTo(final Writer out, final T value) {
        if (value == null) {
            out.writeClassName(null);
            out.writeNull();
        } else {
            if (out.needWriteClassName()) out.writeClassName(factory.getEntityAlias(value.getClass()));
            factory.loadEncoder(value.getClass()).convertTo(out, value);
        }
    }

    @SuppressWarnings("unchecked")
    public void convertMapTo(final Writer out, final Object... values) {
        if (values == null) {
            out.writeNull();
        } else {
            int count = values.length - values.length % 2;
            out.writeMapB(count / 2);
            for (int i = 0; i < count; i += 2) {
                if (i > 0) out.writeArrayMark();
                this.convertTo(out, (T) values[i]);
                out.writeMapMark();
                Object val = values[i + 1];
                if (val instanceof CompletableFuture) {
                    this.convertTo(out, (T) ((CompletableFuture) val).join());
                } else {
                    this.convertTo(out, (T) val);
                }
            }
            out.writeMapE();
        }
    }

    @Override
    public Type getType() {
        return Object.class;
    }

}
