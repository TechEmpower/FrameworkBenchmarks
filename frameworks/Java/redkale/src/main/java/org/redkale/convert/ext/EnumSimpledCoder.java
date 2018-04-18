/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.ext;

import org.redkale.convert.Reader;
import org.redkale.convert.SimpledCoder;
import org.redkale.convert.Writer;

/**
 * 枚举 的SimpledCoder实现
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 * @param <E> Enum的子类
 */
public final class EnumSimpledCoder<R extends Reader, W extends Writer, E extends Enum> extends SimpledCoder<R, W, E> {

    private final Class<E> type;

    public EnumSimpledCoder(Class<E> type) {
        this.type = type;
    }

    @Override
    public void convertTo(final W out, final E value) {
        if (value == null) {
            out.writeNull();
        } else {
            out.writeSmallString(value.toString());
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public E convertFrom(final R in) {
        String value = in.readSmallString();
        if (value == null) return null;
        return (E) Enum.valueOf(type, value);
    }

}
