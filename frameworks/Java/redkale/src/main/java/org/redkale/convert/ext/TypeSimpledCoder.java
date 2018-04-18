/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.ext;

import org.redkale.convert.Reader;
import org.redkale.convert.Writer;
import org.redkale.convert.SimpledCoder;

/**
 * Type 的SimpledCoder实现 只支持Type的子类Class
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 */
public class TypeSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, Class> {

    public static final TypeSimpledCoder instance = new TypeSimpledCoder();

    @Override
    public void convertTo(final W out, final Class value) {
        if (value == null) {
            out.writeNull();
        } else {
            out.writeSmallString(value.getName());
        }
    }

    @Override
    public Class convertFrom(R in) {
        String str = in.readSmallString();
        if (str == null) return null;
        try {
            return Thread.currentThread().getContextClassLoader().loadClass(str);
        } catch (Throwable e) {
            return null;
        }
    }

}
