/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.reflect.Type;

/**
 * 文本序列化/反序列化操作类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类
 * @param <W> Writer输出的子类
 */
public abstract class TextConvert<R extends Reader, W extends Writer> extends Convert<R, W> {

    protected TextConvert(ConvertFactory<R, W> factory) {
        super(factory);
    }

    @Override
    public final boolean isBinary() {
        return false;
    }

    public abstract String convertTo(final Object value);

    public abstract String convertTo(final Type type, final Object value);

    public abstract String convertMapTo(final Object... values);
}
