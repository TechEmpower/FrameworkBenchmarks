/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.util;

import java.nio.*;
import java.nio.charset.*;
import java.util.*;

/**
 * 简单的byte[]操作类。
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public final class ByteArray {

    private byte[] content;

    private int count;

    public ByteArray() {
        this(1024);
    }

    public ByteArray(int size) {
        content = new byte[Math.max(128, size)];
    }

    /**
     * 清空数据,将count置为0,并不清掉byte[]的内容
     */
    public void clear() {
        this.count = 0;
    }

    /**
     * 比较内容是否相同
     *
     * @param bytes 待比较内容
     *
     * @return 是否相同
     */
    public boolean equal(final byte[] bytes) {
        if (bytes == null || count != bytes.length) return false;
        for (int i = 0; i < count; i++) {
            if (content[i] != bytes[i]) return false;
        }
        return true;
    }

    /**
     * 判断内容是否为空
     *
     * @return 是否为空
     */
    public boolean isEmpty() {
        return count == 0;
    }

    /**
     * 获取字节长度
     *
     * @return 长度
     */
    public int size() {
        return count;
    }

    /**
     * 获取指定位置的byte值,须确保0 &lt;= index &lt; size
     *
     * @param index 位置
     *
     * @return byte值
     */
    public byte get(int index) {
        return content[index];
    }

    /**
     * 获取最后一个字节值,调用前须保证count大于0
     *
     * @return byte值
     */
    public byte getLastByte() {
        return content[count - 1];
    }

    /**
     * 将buf内容覆盖到本对象内容中
     *
     * @param buf 目标容器
     */
    public void copyTo(byte[] buf) {
        System.arraycopy(this.content, 0, buf, 0, count);
    }

    /**
     * 将array的内容引用复制给本对象
     *
     * @param array ByteArray
     */
    public void directFrom(ByteArray array) {
        if (array != null) {
            this.content = array.content;
            this.count = array.count;
        }
    }

    /**
     * 将本对象的内容引用复制给array
     *
     * @param array ByteArray
     */
    public void directTo(ByteArray array) {
        if (array != null) {
            array.content = this.content;
            array.count = this.count;
        }
    }

    /**
     * 直接获取全部数据, 实际数据需要根据size长度来截取
     *
     * @return byte[]
     */
    public byte[] directBytes() {
        return content;
    }

    /**
     * 获取byte[]
     *
     * @return byte[]
     */
    public byte[] getBytes() {
        return Arrays.copyOf(content, count);
    }

    /**
     * 获取byte[]并清空
     *
     * @return byte[]
     */
    public byte[] getBytesAndClear() {
        byte[] bs = Arrays.copyOf(content, count);
        clear();
        return bs;
    }

    /**
     * 查找指定值第一次出现的位置,没有返回-1
     *
     * @param value 查询值
     *
     * @return 所在位置
     */
    public int find(byte value) {
        return find(0, value);
    }

    /**
     * 从指定的起始位置查询value值出现的位置,没有返回-1
     *
     * @param offset 起始位置
     * @param value  查询值
     *
     * @return 所在位置
     */
    public int find(int offset, char value) {
        return find(offset, (byte) value);
    }

    /**
     * 从指定的起始位置查询value值出现的位置,没有返回-1
     *
     * @param offset 起始位置
     * @param value  查询值
     *
     * @return 所在位置
     */
    public int find(int offset, byte value) {
        return find(offset, -1, value);
    }

    /**
     * 从指定的起始位置和长度查询value值出现的位置,没有返回-1
     *
     * @param offset 起始位置
     * @param limit  长度限制
     * @param value  查询值
     *
     * @return 所在位置
     */
    public int find(int offset, int limit, char value) {
        return find(offset, limit, (byte) value);
    }

    /**
     * 从指定的起始位置和长度查询value值出现的位置,没有返回-1
     *
     * @param offset 起始位置
     * @param limit  长度限制
     * @param value  查询值
     *
     * @return 所在位置
     */
    public int find(int offset, int limit, byte value) {
        byte[] bytes = this.content;
        int end = limit > 0 ? limit : count;
        for (int i = offset; i < end; i++) {
            if (bytes[i] == value) return i;
        }
        return -1;
    }

    /**
     * 移除最后一个字节
     */
    public void removeLastByte() {
        if (count > 0) count--;
    }

    /**
     * 写入一个int值
     *
     * @param value int值
     */
    public void writeInt(int value) {
        write((byte) (value >> 24 & 0xFF),
            (byte) (value >> 16 & 0xFF),
            (byte) (value >> 8 & 0xFF),
            (byte) (value & 0xFF));
    }

    /**
     * 写入一个byte值
     *
     * @param value byte值
     */
    public void write(byte value) {
        if (count >= content.length - 1) {
            byte[] ns = new byte[content.length + 8];
            System.arraycopy(content, 0, ns, 0, count);
            this.content = ns;
        }
        content[count++] = value;
    }

    /**
     * 写入一组byte值
     *
     * @param values 一组byte值
     */
    public void write(byte... values) {
        if (count >= content.length - values.length) {
            byte[] ns = new byte[content.length + values.length];
            System.arraycopy(content, 0, ns, 0, count);
            this.content = ns;
        }
        System.arraycopy(values, 0, content, count, values.length);
        count += values.length;
    }

    /**
     * 写入ByteBuffer指定长度的数据
     *
     * @param buffer 数据
     * @param len    指定长度
     */
    public void write(ByteBuffer buffer, int len) {
        if (len < 1) return;
        if (count >= content.length - len) {
            byte[] ns = new byte[content.length + len];
            System.arraycopy(content, 0, ns, 0, count);
            this.content = ns;
        }
        buffer.get(content, count, len);
        count += len;
    }

    @Override
    public String toString() {
        return new String(content, 0, count);
    }

    /**
     * 按指定字符集转成字符串
     *
     * @param charset 字符集
     *
     * @return 字符串
     */
    public String toString(final Charset charset) {
        return toString(0, count, charset);
    }

    /**
     * 按指定字符集转成字符串并清空数据
     *
     * @param charset 字符集
     *
     * @return 字符串
     */
    public String toStringAndClear(final Charset charset) {
        String str = toString(0, count, charset);
        clear();
        return str;
    }

    /**
     * 将指定的起始位置和长度按指定字符集转成字符串
     *
     * @param offset  起始位置
     * @param len     长度
     * @param charset 字符集
     *
     * @return 字符串
     */
    public String toString(final int offset, int len, final Charset charset) {
        if (charset == null) return new String(Utility.decodeUTF8(content, offset, len));
        return new String(content, offset, len, charset);
    }

    /**
     * 将指定的起始位置和长度按指定字符集并转义后转成字符串
     *
     * @param offset  起始位置
     * @param len     长度
     * @param charset 字符集
     *
     * @return 字符串
     */
    public String toDecodeString(final int offset, int len, final Charset charset) {
        int start = offset;
        final int end = offset + len;
        boolean flag = false; //是否需要转义
        byte[] bs = content;
        for (int i = offset; i < end; i++) {
            if (content[i] == '+' || content[i] == '%') {
                flag = true;
                break;
            }
        }
        if (flag) {
            int index = 0;
            bs = new byte[len];
            for (int i = offset; i < end; i++) {
                switch (content[i]) {
                    case '+':
                        bs[index] = ' ';
                        break;
                    case '%':
                        bs[index] = (byte) ((hexBit(content[++i]) * 16 + hexBit(content[++i])));
                        break;
                    default:
                        bs[index] = content[i];
                        break;
                }
                index++;
            }
            start = 0;
            len = index;
        }
        if (charset == null) return new String(Utility.decodeUTF8(bs, start, len));
        return new String(bs, start, len, charset);
    }

    private static int hexBit(byte b) {
        if ('0' <= b && '9' >= b) return b - '0';
        if ('a' <= b && 'z' >= b) return b - 'a' + 10;
        if ('A' <= b && 'Z' >= b) return b - 'A' + 10;
        return b;
    }

}
