/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.ext;

import org.redkale.convert.SimpledCoder;
import org.redkale.convert.Writer;
import org.redkale.convert.Reader;
import java.math.BigInteger;

/**
 * BigInteger 的SimpledCoder实现
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 */
public final class BigIntegerSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, BigInteger> {

    public static final BigIntegerSimpledCoder instance = new BigIntegerSimpledCoder();

    @Override
    @SuppressWarnings("unchecked")
    public void convertTo(W out, BigInteger value) {
        if (value == null) {
            out.writeNull();
            return;
        }
        ByteArraySimpledCoder.instance.convertTo(out, value.toByteArray());
    }

    @Override
    @SuppressWarnings("unchecked")
    public BigInteger convertFrom(R in) {
        byte[] bytes = ByteArraySimpledCoder.instance.convertFrom(in);
        return bytes == null ? null : new BigInteger(bytes);
    }

    /**
     * BigInteger 的JsonSimpledCoder实现
     *
     * @param <R> Reader输入的子类型
     * @param <W> Writer输出的子类型
     */
    public static class BigIntegerJsonSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, BigInteger> {

        public static final BigIntegerJsonSimpledCoder instance = new BigIntegerJsonSimpledCoder();

        @Override
        public void convertTo(final Writer out, final BigInteger value) {
            if (value == null) {
                out.writeNull();
            } else {
                out.writeString(value.toString());
            }
        }

        @Override
        public BigInteger convertFrom(Reader in) {
            final String str = in.readString();
            if (str == null) return null;
            return new BigInteger(str);
        }
    }
}
