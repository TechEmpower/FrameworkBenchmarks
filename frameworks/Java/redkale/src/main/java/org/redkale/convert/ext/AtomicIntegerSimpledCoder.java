/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.ext;

import java.util.concurrent.atomic.AtomicInteger;
import org.redkale.convert.*;

/**
 * AtomicInteger 的SimpledCoder实现
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 */
public class AtomicIntegerSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, AtomicInteger> {

    public static final AtomicIntegerSimpledCoder instance = new AtomicIntegerSimpledCoder();

    @Override
    public void convertTo(W out, AtomicInteger value) {
        out.writeInt(value == null ? 0 : value.get());
    }

    @Override
    public AtomicInteger convertFrom(R in) {
        return new AtomicInteger(in.readInt());
    }

}
