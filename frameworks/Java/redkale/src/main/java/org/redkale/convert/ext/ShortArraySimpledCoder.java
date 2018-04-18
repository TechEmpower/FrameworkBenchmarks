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
 * short[] 的SimpledCoder实现
 *
 * <p> 详情见: https://redkale.org
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 */
public final class ShortArraySimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, short[]> {

    public static final ShortArraySimpledCoder instance = new ShortArraySimpledCoder();

    @Override
    public void convertTo(W out, short[] values) {
        if (values == null) {
            out.writeNull();
            return;
        }
        out.writeArrayB(values.length);
        boolean flag = false;
        for (short v : values) {
            if (flag) out.writeArrayMark();
            out.writeShort(v);
            flag = true;
        }
        out.writeArrayE();
    }

    @Override
    public short[] convertFrom(R in) {
        int len = in.readArrayB();
        if (len == Reader.SIGN_NULL) return null;
        if (len == Reader.SIGN_NOLENGTH) {
            int size = 0;
            short[] data = new short[8];
            while (in.hasNext()) {
                if (size >= data.length) {
                    short[] newdata = new short[data.length + 4];
                    System.arraycopy(data, 0, newdata, 0, size);
                    data = newdata;
                }
                data[size++] = in.readShort();
            }
            in.readArrayE();
            short[] newdata = new short[size];
            System.arraycopy(data, 0, newdata, 0, size);
            return newdata;
        } else {
            short[] values = new short[len];
            for (int i = 0; i < values.length; i++) {
                values[i] = in.readShort();
            }
            in.readArrayE();
            return values;
        }
    }

}
