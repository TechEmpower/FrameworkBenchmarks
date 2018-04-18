/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.reflect.Type;

/**
 * 二进制序列化/反序列化操作类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类
 * @param <W> Writer输出的子类
 */
public abstract class BinaryConvert<R extends Reader, W extends Writer> extends Convert<R, W> {

    protected BinaryConvert(ConvertFactory<R, W> factory) {
        super(factory);
    }

    @Override
    public final boolean isBinary() {
        return true;
    }

    public abstract byte[] convertTo(final Object value);

    public abstract byte[] convertTo(final Type type, final Object value);

    public abstract byte[] convertMapTo(final Object... values);
}
