/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.json;

import java.io.*;
import java.nio.*;
import java.nio.charset.*;
import org.redkale.convert.*;
import org.redkale.util.*;

/**
 *
 * 详情见: https://redkale.org
 * 
 * @author zhangjx
 */
class JsonStreamWriter extends JsonByteBufferWriter {

    private OutputStream out;

    protected JsonStreamWriter(boolean tiny, OutputStream out) {
        this(tiny, null, out);
    }

    protected JsonStreamWriter(boolean tiny, Charset charset, OutputStream out) {
        super(tiny, charset, null);
        this.out = out;
    }

    @Override
    protected boolean recycle() {
        super.recycle();
        this.out = null;
        return false;
    }

    @Override
    public void writeTo(final char ch) {
        if (ch > Byte.MAX_VALUE) throw new ConvertException("writeTo char(int.value = " + (int) ch + ") must be less 127");
        try {
            out.write((byte) ch);
        } catch (IOException e) {
            throw new ConvertException(e);
        }
    }

    @Override
    public void writeTo(final char[] chs, final int start, final int len) {
        writeTo(false, chs, start, len);
    }

    private void writeTo(final boolean quote, final char[] chs, final int start, final int len) {
        try {
            if (quote) out.write('"');
            if (charset == null) { //UTF-8
                final int limit = start + len;
                for (int i = start; i < limit; i++) {
                    char c = chs[i];
                    if (c < 0x80) {
                        out.write((byte) c);
                    } else if (c < 0x800) {
                        out.write((byte) (0xc0 | (c >> 6)));
                        out.write((byte) (0x80 | (c & 0x3f)));
                    } else {
                        out.write((byte) (0xe0 | ((c >> 12))));
                        out.write((byte) (0x80 | ((c >> 6) & 0x3f)));
                        out.write((byte) (0x80 | (c & 0x3f)));
                    }
                }
            } else {
                ByteBuffer bb = charset.encode(CharBuffer.wrap(chs, start, len));
                out.write(bb.array());
            }
            if (quote) out.write('"');
        } catch (IOException e) {
            throw new ConvertException(e);
        }
    }

    /**
     * <b>注意：</b> 该String值不能为null且不会进行转义， 只用于不含需要转义字符的字符串，例如enum、double、BigInteger转换的String
     *
     * @param quote 是否写入双引号
     * @param value String值
     */
    @Override
    public void writeTo(final boolean quote, final String value) {
        char[] chs = Utility.charArray(value);
        writeTo(quote, chs, 0, chs.length);
    }

    @Override
    public void writeInt(int value) {
        writeTo(false, String.valueOf(value));
    }

    @Override
    public void writeLong(long value) {
        writeTo(false, String.valueOf(value));
    }

    @Override
    public void writeString(String value) {
        if (value == null) {
            writeNull();
            return;
        }
        final char[] chs = Utility.charArray(value);
        int len = 0;
        for (char ch : chs) {
            switch (ch) {
                case '\n': len += 2;
                    break;
                case '\r': len += 2;
                    break;
                case '\t': len += 2;
                    break;
                case '\\': len += 2;
                    break;
                case '"': len += 2;
                    break;
                default: len++;
                    break;
            }
        }
        if (len == chs.length) {
            writeTo(true, chs, 0, len);
            return;
        }
        StringBuilder sb = new StringBuilder(len);
        for (char ch : chs) {
            switch (ch) {
                case '\n': sb.append("\\n");
                    break;
                case '\r': sb.append("\\r");
                    break;
                case '\t': sb.append("\\t");
                    break;
                case '\\': sb.append("\\\\");
                    break;
                case '"': sb.append("\\\"");
                    break;
                default: sb.append(ch);
                    break;
            }
        }
        char[] cs = Utility.charArray(sb);
        writeTo(true, cs, 0, sb.length());
    }
}
