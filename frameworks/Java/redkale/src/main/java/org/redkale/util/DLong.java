/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.util;

import java.nio.*;
import java.util.*;

/**
 * 16bytes数据结构
 * 注意： 为了提高性能， DLong中的bytes是直接返回， 不得对bytes的内容进行修改。
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public final class DLong extends Number implements Comparable<DLong> {

    public static final DLong ZERO = new DLong(new byte[16]);

    protected final byte[] value;

    protected DLong(long v1, long v2) {  //暂时不用
        this.value = new byte[]{(byte) (v1 >> 56), (byte) (v1 >> 48), (byte) (v1 >> 40), (byte) (v1 >> 32),
            (byte) (v1 >> 24), (byte) (v1 >> 16), (byte) (v1 >> 8), (byte) v1, (byte) (v2 >> 56), (byte) (v2 >> 48), (byte) (v2 >> 40), (byte) (v2 >> 32),
            (byte) (v2 >> 24), (byte) (v2 >> 16), (byte) (v2 >> 8), (byte) v2};
    }

    protected DLong(byte[] bytes) {
        if (bytes == null || bytes.length != 16) throw new NumberFormatException("Not 16 length bytes");
        this.value = bytes;
    }

    public byte[] getBytes() {
        return Arrays.copyOf(value, value.length);
    }

    public byte[] directBytes() {
        return value;
    }

    public static DLong create(byte[] bytes) {
        return new DLong(bytes);
    }

    public static DLong read(ByteBuffer buffer) {
        byte[] bs = new byte[16];
        buffer.get(bs);
        return new DLong(bs);
    }

    public static ByteBuffer write(ByteBuffer buffer, DLong dlong) {
        buffer.put(dlong.value);
        return buffer;
    }

    public boolean equals(byte[] bytes) {
        return Arrays.equals(this.value, bytes);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        final DLong other = (DLong) obj;
        return Arrays.equals(this.value, other.value);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(value);
    }

    @Override
    public String toString() {
        if (this == ZERO) return "0";
        return new String(Utility.binToHex(value));
    }

    @Override
    public int intValue() {
        return ((value[12] & 0xff) << 24) | ((value[13] & 0xff) << 16) | ((value[14] & 0xff) << 8) | (value[15] & 0xff);
    }

    @Override
    public long longValue() {
        return ((((long) value[8] & 0xff) << 56)
                | (((long) value[9] & 0xff) << 48)
                | (((long) value[10] & 0xff) << 40)
                | (((long) value[11] & 0xff) << 32)
                | (((long) value[12] & 0xff) << 24)
                | (((long) value[13] & 0xff) << 16)
                | (((long) value[14] & 0xff) << 8)
                | (((long) value[15] & 0xff)));
    }

    @Override
    public float floatValue() {
        return (float) longValue();
    }

    @Override
    public double doubleValue() {
        return (double) longValue();
    }

    @Override
    public int compareTo(DLong o) {
        if (o == null) return 1;
        for (int i = 0; i < value.length; i++) {
            if (this.value[i] != o.value[i]) return this.value[i] - o.value[i];
        }
        return 0;
    }

}
