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
 * short 的SimpledCoder实现
 *
 * <p> 详情见: https://redkale.org
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 */
public final class ShortSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, Short> {

    public static final ShortSimpledCoder instance = new ShortSimpledCoder();

    @Override
    public void convertTo(W out, Short value) {
        out.writeShort(value);
    }

    @Override
    public Short convertFrom(R in) {
        return in.readShort();
    }

}
