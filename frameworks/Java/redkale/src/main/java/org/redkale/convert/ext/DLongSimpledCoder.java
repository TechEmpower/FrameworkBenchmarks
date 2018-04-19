/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.ext;

import org.redkale.convert.Reader;
import org.redkale.convert.Writer;
import org.redkale.convert.SimpledCoder;
import org.redkale.util.*;

/**
 * Dlong 的SimpledCoder实现
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 */
public final class DLongSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, DLong> {

    private static final ByteArraySimpledCoder bsSimpledCoder = ByteArraySimpledCoder.instance;

    public static final DLongSimpledCoder instance = new DLongSimpledCoder();

    @Override
    @SuppressWarnings("unchecked")
    public void convertTo(final W out, final DLong value) {
        if (value == null) {
            out.writeNull();
        } else {
            bsSimpledCoder.convertTo(out, value.directBytes());
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public DLong convertFrom(R in) {
        byte[] bs = bsSimpledCoder.convertFrom(in);
        if (bs == null) return null;
        return DLong.create(bs);
    }

    /**
     * DLong 的JsonSimpledCoder实现
     *
     * @param <R> Reader输入的子类型
     * @param <W> Writer输出的子类型
     */
    public static class DLongJsonSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, DLong> {

        public static final DLongJsonSimpledCoder instance = new DLongJsonSimpledCoder();

        @Override
        public void convertTo(final Writer out, final DLong value) {
            if (value == null) {
                out.writeNull();
            } else {
                out.writeSmallString(value.toString());
            }
        }

        @Override
        public DLong convertFrom(Reader in) {
            final String str = in.readSmallString();
            if (str == null) return null;
            return DLong.create(Utility.hexToBin(str));
        }
    }
}
