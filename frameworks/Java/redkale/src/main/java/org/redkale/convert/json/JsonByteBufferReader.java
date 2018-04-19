/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.json;

import java.nio.*;
import java.nio.charset.*;
import org.redkale.convert.*;
import static org.redkale.convert.Reader.*;

/**
 * 以ByteBuffer为数据载体的JsonReader   <br>
 *
 * 只支持UTF-8格式
 *
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class JsonByteBufferReader extends JsonReader {

    private char currentChar;

    private ByteBuffer[] buffers;

    private int currentIndex = 0;

    private ByteBuffer currentBuffer;

    protected ConvertMask mask;

    protected JsonByteBufferReader(ConvertMask mask, ByteBuffer... buffers) {
        this.mask = mask;
        this.buffers = buffers;
        if (buffers != null && buffers.length > 0) this.currentBuffer = buffers[currentIndex];
    }

    @Override
    protected boolean recycle() {
        super.recycle();   // this.position 初始化值为-1
        this.currentIndex = 0;
        this.currentChar = 0;
        this.currentBuffer = null;
        this.buffers = null;
        this.mask = null;
        return false;
    }

    protected byte nextByte() {
        if (this.currentBuffer.hasRemaining()) {
            this.position++;
            return mask == null ? this.currentBuffer.get() : mask.unmask(this.currentBuffer.get());
        }
        for (;;) {
            this.currentBuffer = this.buffers[++this.currentIndex];
            if (this.currentBuffer.hasRemaining()) {
                this.position++;
                return mask == null ? this.currentBuffer.get() : mask.unmask(this.currentBuffer.get());
            }
        }
    }

    /**
     * 读取下一个字符， 不跳过空白字符
     *
     * @return 有效字符或空白字符
     */
    @Override
    protected final char nextChar() {
        if (currentChar != 0) {
            char ch = currentChar;
            this.currentChar = 0;
            return ch;
        }
        if (this.currentBuffer != null) {
            int remain = this.currentBuffer.remaining();
            if (remain == 0 && this.currentIndex + 1 >= this.buffers.length) return 0;
        }
        byte b1 = nextByte();
        if (b1 >= 0) {// 1 byte, 7 bits: 0xxxxxxx
            return (char) b1;
        } else if ((b1 >> 5) == -2 && (b1 & 0x1e) != 0) { // 2 bytes, 11 bits: 110xxxxx 10xxxxxx
            return (char) (((b1 << 6) ^ nextByte()) ^ (((byte) 0xC0 << 6) ^ ((byte) 0x80)));
        } else if ((b1 >> 4) == -2) { // 3 bytes, 16 bits: 1110xxxx 10xxxxxx 10xxxxxx
            return (char) ((b1 << 12) ^ (nextByte() << 6) ^ (nextByte() ^ (((byte) 0xE0 << 12) ^ ((byte) 0x80 << 6) ^ ((byte) 0x80))));
        } else { // 4 bytes, 21 bits: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
            throw new RuntimeException(new UnmappableCharacterException(4));
        }
    }

    /**
     * 读取下一个有效字符
     *
     * @return 有效字符
     */
    @Override
    protected final char nextGoodChar() {
        char c = nextChar();
        if (c > ' ' || c == 0) return c; // 0 表示buffer结尾了
        for (;;) {
            c = nextChar();
            if (c > ' ' || c == 0) return c;
        }
    }

    /**
     * 回退最后读取的字符
     *
     * @param ch 回退的字符
     */
    @Override
    protected final void backChar(char ch) {
        this.currentChar = ch;
    }

    /**
     * 判断下一个非空白字符是否为{
     *
     * @return SIGN_NOLENGTH 或 SIGN_NULL
     */
    @Override
    public final String readObjectB(final Class clazz) {
        char ch = nextGoodChar();
        if (ch == '{') return "";
        if (ch == 'n' && nextChar() == 'u' && nextChar() == 'l' && nextChar() == 'l') return null;
        if (ch == 'N' && nextChar() == 'U' && nextChar() == 'L' && nextChar() == 'L') return null;
        throw new ConvertException("a json object text must begin with '{' (position = " + position + ") but '" + ch + "'");
    }

    /**
     * 判断下一个非空白字符是否为[
     *
     * @return SIGN_NOLENGTH 或 SIGN_NULL
     */
    @Override
    public final int readArrayB() {
        char ch = nextGoodChar();
        if (ch == '[' || ch == '{') return SIGN_NOLENGTH;
        if (ch == 'n' && nextChar() == 'u' && nextChar() == 'l' && nextChar() == 'l') return SIGN_NULL;
        if (ch == 'N' && nextChar() == 'U' && nextChar() == 'L' && nextChar() == 'L') return SIGN_NULL;
        throw new ConvertException("a json array text must begin with '[' (position = " + position + ") but '" + ch + "'");
    }

    /**
     * 判断下一个非空白字符是否:
     */
    @Override
    public final void readBlank() {
        char ch = nextGoodChar();
        if (ch == ':') return;
        throw new ConvertException("expected a ':' but '" + ch + "'(position = " + position + ")");
    }

    /**
     * 判断对象是否存在下一个属性或者数组是否存在下一个元素
     *
     * @return 是否存在
     */
    @Override
    public final boolean hasNext() {
        char ch = nextGoodChar();
        if (ch == ',') return true;
        if (ch == '}' || ch == ']' || ch == 0) return false;
        backChar(ch); // { [ 交由 readObjectB 或 readMapB 或 readArrayB 读取
        return true;
    }

    /**
     * 读取小字符串
     *
     * @return String值
     */
    @Override
    public final String readSmallString() {
        char ch = nextGoodChar();
        if (ch == 0) return null;
        final StringBuilder sb = new StringBuilder();
        if (ch == '"' || ch == '\'') {
            final char quote = ch;
            for (;;) {
                ch = nextChar();
                if (ch == '\\') {
                    char c = nextChar();
                    switch (c) {
                        case '"':
                        case '\'':
                        case '\\':
                        case '/':
                            sb.append(c);
                            break;
                        case 'n':
                            sb.append('\n');
                            break;
                        case 'r':
                            sb.append('\r');
                            break;
                        case 'u':
                            sb.append((char) Integer.parseInt(new String(new char[]{nextChar(), nextChar(), nextChar(), nextChar()}), 16));
                            break;
                        case 't':
                            sb.append('\t');
                            break;
                        case 'b':
                            sb.append('\b');
                            break;
                        case 'f':
                            sb.append('\f');
                            break;
                        default:
                            throw new ConvertException("illegal escape(" + c + ") (position = " + this.position + ")");
                    }
                } else if (ch == quote || ch == 0) {
                    break;
                } else {
                    sb.append(ch);
                }
            }
            return sb.toString();
        } else {
            sb.append(ch);
            for (;;) {
                ch = nextChar();
                if (ch == '\\') {
                    char c = nextChar();
                    switch (c) {
                        case '"':
                        case '\'':
                        case '\\':
                        case '/':
                            sb.append(c);
                            break;
                        case 'n':
                            sb.append('\n');
                            break;
                        case 'r':
                            sb.append('\r');
                            break;
                        case 'u':
                            sb.append((char) Integer.parseInt(new String(new char[]{nextChar(), nextChar(), nextChar(), nextChar()}), 16));
                            break;
                        case 't':
                            sb.append('\t');
                            break;
                        case 'b':
                            sb.append('\b');
                            break;
                        case 'f':
                            sb.append('\f');
                            break;
                        default:
                            throw new ConvertException("illegal escape(" + c + ") (position = " + this.position + ")");
                    }
                } else if (ch == ',' || ch == ']' || ch == '}' || ch <= ' ' || ch == ':') { //  ch <= ' ' 包含 0
                    backChar(ch);
                    break;
                } else {
                    sb.append(ch);
                }
            }
            String rs = sb.toString();
            return "null".equalsIgnoreCase(rs) ? null : rs;
        }
    }

    /**
     * 读取一个int值
     *
     * @return int值
     */
    @Override
    public final int readInt() {
        char firstchar = nextGoodChar();
        if (firstchar == '"' || firstchar == '\'') {
            firstchar = nextChar();
            if (firstchar == '"' || firstchar == '\'') return 0;
        }
        int value = 0;
        final boolean negative = firstchar == '-';
        if (!negative) {
            if (firstchar < '0' || firstchar > '9') throw new ConvertException("illegal escape(" + firstchar + ") (position = " + position + ")");
            value = firstchar - '0';
        }
        for (;;) {
            char ch = nextChar();
            if (ch == 0) break;
            if (ch >= '0' && ch <= '9') {
                value = (value << 3) + (value << 1) + (ch - '0');
            } else if (ch == '"' || ch == '\'') {
            } else if (ch == ',' || ch == '}' || ch == ']' || ch <= ' ' || ch == ':') {
                backChar(ch);
                break;
            } else {
                throw new ConvertException("illegal escape(" + ch + ") (position = " + position + ")");
            }
        }
        return negative ? -value : value;
    }

    /**
     * 读取一个long值
     *
     * @return long值
     */
    @Override
    public final long readLong() {
        char firstchar = nextGoodChar();
        if (firstchar == '"' || firstchar == '\'') {
            firstchar = nextChar();
            if (firstchar == '"' || firstchar == '\'') return 0L;
        }
        long value = 0;
        final boolean negative = firstchar == '-';
        if (!negative) {
            if (firstchar < '0' || firstchar > '9') throw new ConvertException("illegal escape(" + firstchar + ") (position = " + position + ")");
            value = firstchar - '0';
        }
        for (;;) {
            char ch = nextChar();
            if (ch == 0) break;
            if (ch >= '0' && ch <= '9') {
                value = (value << 3) + (value << 1) + (ch - '0');
            } else if (ch == '"' || ch == '\'') {
            } else if (ch == ',' || ch == '}' || ch == ']' || ch <= ' ' || ch == ':') {
                backChar(ch);
                break;
            } else {
                throw new ConvertException("illegal escape(" + ch + ") (position = " + position + ")");
            }
        }
        return negative ? -value : value;
    }

    /**
     * 读取字符串， 必须是"或者'包围的字符串值
     *
     * @return String值
     */
    @Override
    public final String readString() {
        return readSmallString();
    }

}
