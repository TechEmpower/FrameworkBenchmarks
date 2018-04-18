/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.ext;

import java.util.concurrent.atomic.AtomicLong;
import org.redkale.convert.*;

/**
 * AtomicLong 的SimpledCoder实现
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 */
public final class AtomicLongSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, AtomicLong> {

    public static final AtomicLongSimpledCoder instance = new AtomicLongSimpledCoder();

    @Override
    public void convertTo(W out, AtomicLong value) {
        out.writeLong(value == null ? 0 : value.get());
    }

    @Override
    public AtomicLong convertFrom(R in) {
        return new AtomicLong(in.readLong());
    }

}
