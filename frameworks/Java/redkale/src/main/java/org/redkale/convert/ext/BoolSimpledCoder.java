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
 * boolean 的SimpledCoder实现
 * 
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 */
public final class BoolSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, Boolean> {

    public static final BoolSimpledCoder instance = new BoolSimpledCoder();

    @Override
    public void convertTo(W out, Boolean value) {
        out.writeBoolean(value);
    }

    @Override
    public Boolean convertFrom(R in) {
        return in.readBoolean();
    }

}
