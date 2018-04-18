/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import org.redkale.util.Utility;
import java.io.*;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.AbstractMap;
import java.util.function.*;
import java.util.logging.*;
import org.redkale.convert.*;
import org.redkale.net.Cryptor;

/**
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public final class WebSocketPacket {

    static final WebSocketPacket NONE = new WebSocketPacket();

    public static final WebSocketPacket DEFAULT_PING_PACKET = new WebSocketPacket(FrameType.PING, new byte[0]);

    public static enum MessageType {
        STRING, BYTES, OBJECT;
    }

    public static enum FrameType {

        TEXT(0x01), BINARY(0x02), CLOSE(0x08), PING(0x09), PONG(0x0A);

        private final int value;

        private FrameType(int v) {
            this.value = v;
        }

        public int getValue() {
            return value;
        }

        public static FrameType valueOf(int v) {
            switch (v) {
                case 0x01: return TEXT;
                case 0x02: return BINARY;
                case 0x08: return CLOSE;
                case 0x09: return PING;
                case 0x0A: return PONG;
                default: return null;
            }
        }
    }

    protected FrameType type;

    protected String payload;

    protected byte[] bytes;

    protected boolean last = true;

    //---------------发送------------------------
    Object sendJson;

    Convert sendConvert;

    boolean sendMapconvable;

    ByteBuffer[] sendBuffers;

    //---------------接收------------------------
    MessageType receiveType;

    int receiveCount;

    int receiveLength;

    Object receiveMessage;

    ConvertMask receiveMasker;

    ByteBuffer[] receiveBuffers;

    public WebSocketPacket() {
    }

    public WebSocketPacket(String payload) {
        this(payload, true);
    }

    public WebSocketPacket(String payload, boolean fin) {
        this.type = FrameType.TEXT;
        this.payload = payload;
        this.last = fin;
    }

    public WebSocketPacket(byte[] data) {
        this(FrameType.BINARY, data, true);
    }

    public WebSocketPacket(byte[] data, boolean fin) {
        this(FrameType.BINARY, data, fin);
    }

    public WebSocketPacket(FrameType type, byte[] data) {
        this(type, data, true);
    }

    public WebSocketPacket(FrameType type, byte[] data, boolean fin) {
        this.type = type;
        if (type == FrameType.TEXT) {
            this.payload = new String(Utility.decodeUTF8(data));
        } else {
            this.bytes = data;
        }
        this.last = fin;
    }

    public WebSocketPacket(Serializable message, boolean fin) {
        boolean bin = message != null && message.getClass() == byte[].class;
        if (bin) {
            this.type = FrameType.BINARY;
            this.bytes = (byte[]) message;
        } else {
            this.type = FrameType.TEXT;
            this.payload = String.valueOf(message);
        }
        this.last = fin;
    }

    WebSocketPacket(Convert convert, boolean mapconvable, Object json, boolean fin) {
        this.type = (convert == null || !convert.isBinary()) ? FrameType.TEXT : FrameType.BINARY;
        this.sendConvert = convert;
        this.sendMapconvable = mapconvable;
        this.sendJson = json;
        this.last = fin;
        if (mapconvable && !(json instanceof Object[])) throw new IllegalArgumentException();
    }

    WebSocketPacket(ByteBuffer[] sendBuffers, FrameType type, boolean fin) {
        this.type = type;
        this.last = fin;
        this.setSendBuffers(sendBuffers);
    }

    void setSendBuffers(ByteBuffer[] sendBuffers) {
        this.sendBuffers = sendBuffers;
    }

    ByteBuffer[] duplicateSendBuffers() {
        ByteBuffer[] rs = new ByteBuffer[this.sendBuffers.length];
        for (int i = 0; i < this.sendBuffers.length; i++) {
            rs[i] = this.sendBuffers[i].duplicate().asReadOnlyBuffer(); //必须使用asReadOnlyBuffer， 否则会导致ByteBuffer对应的byte[]被ObjectPool回收两次
        }
        return rs;
    }

    public byte[] content() {
        if (this.type == FrameType.TEXT) return Utility.encodeUTF8(getPayload());
        if (this.bytes == null) return new byte[0];
        return this.bytes;
    }

    public String getPayload() {
        return payload;
    }

    public byte[] getBytes() {
        return bytes;
    }

    public boolean isLast() {
        return last;
    }

    public FrameType getType() {
        return type;
    }

    public void setType(FrameType type) {
        this.type = type;
    }

    public void setPayload(String payload) {
        this.payload = payload;
    }

    public void setBytes(byte[] bytes) {
        this.bytes = bytes;
    }

    public void setLast(boolean last) {
        this.last = last;
    }

    public String toSimpleString() {
        if (payload != null) return payload;
        if (bytes != null) return "byte[" + bytes.length + "]";
        return this.toString();
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName() + "[type=" + type + ", last=" + last + (payload != null ? (", payload=" + payload) : "") + (bytes != null ? (", bytes=[" + bytes.length + ']') : (receiveLength > 0 ? (", receivemsg=" + (receiveMessage instanceof byte[] ? ("byte[" + ((byte[]) receiveMessage).length + "]") : receiveMessage)) : "")) + (sendJson != null ? (", json=" + (sendMapconvable ? Utility.ofMap((Object[]) sendJson) : sendJson)) : "") + "]";
    }

    /**
     * 消息编码
     *
     * @param supplier Supplier
     * @param cryptor  Cryptor
     *
     * @return ByteBuffer[]
     */
    ByteBuffer[] encode(final Supplier<ByteBuffer> supplier, final Consumer<ByteBuffer> consumer, final Cryptor cryptor) {
        final byte opcode = (byte) (this.type.getValue() | 0x80);
        if (this.sendConvert != null) {
            Supplier<ByteBuffer> newsupplier = new Supplier<ByteBuffer>() {

                private ByteBuffer buf = supplier.get();

                @Override
                public ByteBuffer get() {
                    if (buf != null) {
                        ByteBuffer rs = buf;
                        rs.position(6);
                        this.buf = null;
                        return rs;
                    }
                    return supplier.get();
                }
            };
            ByteBuffer[] buffers = this.sendMapconvable ? this.sendConvert.convertMapTo(newsupplier, (Object[]) sendJson) : this.sendConvert.convertTo(newsupplier, sendJson);
            if (cryptor != null) buffers = cryptor.encrypt(buffers, supplier, consumer);
            int len = 0;
            for (ByteBuffer buf : buffers) {
                len += buf.remaining();
            }
            int contentLength = len - 6;
            ByteBuffer firstbuf = buffers[0];
            if (contentLength <= 0x7D) { //125
                firstbuf.put(4, opcode);
                firstbuf.put(5, (byte) contentLength);
                firstbuf.position(4);
            } else if (contentLength <= 0xFFFF) {
                firstbuf.put(2, opcode);
                firstbuf.put(3, (byte) 0x7E); //126
                firstbuf.putChar(4, (char) contentLength);
                firstbuf.position(2);
            } else {
                firstbuf.put(0, opcode);
                firstbuf.put(1, (byte) 0x7F); //127
                firstbuf.putInt(2, contentLength);
            }
            return buffers;
        }

        ByteBuffer buffer = supplier.get();  //确保ByteBuffer的capacity不能小于128
        byte[] content = content();
        if (cryptor != null) {
            ByteBuffer[] ss = new ByteBuffer[]{ByteBuffer.wrap(content)};
            ByteBuffer[] bs = cryptor.encrypt(ss, supplier, consumer);
            if (bs != ss) {
                int r = 0;
                for (ByteBuffer bb : bs) {
                    r += bb.remaining();
                }
                content = new byte[r];
                int index = 0;
                for (ByteBuffer bb : bs) {
                    int re = bb.remaining();
                    bb.get(content, index, re);
                    index += re;
                }
                for (ByteBuffer bb : bs) {
                    consumer.accept(bb);
                }
            }
        }
        final int len = content.length;
        if (len <= 0x7D) { //125
            buffer.put(opcode);
            buffer.put((byte) len);
            buffer.put(content);
            buffer.flip();
            return new ByteBuffer[]{buffer};
        }
        if (len <= 0xFFFF) { // 65535
            buffer.put(opcode);
            buffer.put((byte) 0x7E); //126
            buffer.putChar((char) len);
        } else {
            buffer.put(opcode);
            buffer.put((byte) 0x7F); //127
            buffer.putInt(len);
        }
        int start = buffer.remaining();
        int pend = len - buffer.remaining();
        if (pend <= 0) {
            buffer.put(content);
            buffer.flip();
            return new ByteBuffer[]{buffer};
        }
        buffer.put(content, 0, buffer.remaining());
        buffer.flip();
        final int capacity = buffer.capacity();
        final ByteBuffer[] buffers = new ByteBuffer[(pend / capacity) + 1 + ((pend % capacity) > 0 ? 1 : 0)];
        buffers[0] = buffer;
        for (int i = 1; i < buffers.length; i++) {
            ByteBuffer buf = supplier.get();
            buf.put(content, start, Math.min(pend, capacity));
            buf.flip();
            buffers[i] = buf;
            start += capacity;
            pend -= capacity;
        }
        return buffers;
    }

//    public static void main(String[] args) throws Throwable {
//        byte[] mask = new byte[]{(byte) 0x8f, (byte) 0xf8, (byte) 0x6d, (byte) 0x94};
//        ByteBuffer buffer = ByteBuffer.wrap(new byte[]{(byte) 0x67, (byte) 0x47, (byte) 0xf4, (byte) 0x70, (byte) 0x37, (byte) 0x52, (byte) 0x8b, (byte) 0x0c, (byte) 0x20, (byte) 0x1e, (byte) 0xdb, (byte) 0x1c, (byte) 0x69, (byte) 0x79, (byte) 0xc2});
//        ConvertMask masker = new ConvertMask() {
//            private int index = 0;
//
//            public byte unmask(byte value) {
//                return (byte) (value ^ mask[index++ % 4]);
//            }
//        };
//        String rs = JsonConvert.root().convertFrom(String.class, masker, buffer);
//        System.out.println(rs);
//    }
    /**
     *
     * @param webSocket  WebSocket
     * @param readBuffer ByteBuffer
     *
     * @return boolean 已接收完返回true， 需要继续接收body返回false；
     */
    boolean receiveBody(WebSocket webSocket, ByteBuffer readBuffer) {
        int need = receiveLength - receiveCount;
        boolean over = readBuffer.remaining() >= need;
        this.receiveBuffers = Utility.append(this.receiveBuffers, readBuffer);
        if (over) parseReceiveMessage(webSocket, this.receiveBuffers);
        return over;
    }

    /**
     * 消息解码  <br>
     *
     * 0 1 2 3
     * 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     * +-+-+-+-+-------+-+-------------+-------------------------------+
     * |F|R|R|R| opcode|M| Payload len | Extended payload length |
     * |I|S|S|S| (4) |A| (7) | (16/64) |
     * |N|V|V|V| |S| | (if payload len==126/127) |
     * | |1|2|3| |K| | |
     * +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
     * | Extended payload length continued, if payload len == 127 |
     * + - - - - - - - - - - - - - - - +-------------------------------+
     * | |Masking-key, if MASK set to 1 |
     * +-------------------------------+-------------------------------+
     * | Masking-key (continued) | Payload Data |
     * +-------------------------------- - - - - - - - - - - - - - - - +
     * : Payload Data continued :
     * + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
     * | Payload Data continued |
     * +-----------------------------------------------------------------------+
     *
     * @param buffer
     * @param exbuffers
     *
     * @return 返回NONE表示Buffer内容不够； 返回this表示解析完成或部分解析完成；返回null表示解析异常；
     */
    WebSocketPacket decode(final Logger logger, final WebSocket webSocket, final int wsmaxbody,
        final AbstractMap.SimpleEntry<String, byte[]> halfBytes, final ByteBuffer buffer) {
        //开始
        final boolean debug = false; //调试开关
        if (debug) logger.log(Level.FINEST, "read websocket message's length = " + buffer.remaining());
        if (!buffer.hasRemaining()) return NONE;
        if (buffer.remaining() < 2) {
            byte[] bs = new byte[buffer.remaining()];
            buffer.get(bs);
            halfBytes.setValue(bs);
            return NONE;
        }
        final byte opcode = buffer.get();   //第一个字节
        this.last = (opcode & 0b1000_0000) != 0;
        this.type = FrameType.valueOf(opcode & 0xF);
        if (type == FrameType.CLOSE) {
            if (debug) logger.log(Level.FINEST, " receive close command from websocket client");
        }
        final boolean checkrsv = false;//暂时不校验
        if (checkrsv && (opcode & 0b0111_0000) != 0) {
            if (debug) logger.log(Level.FINE, "rsv1 rsv2 rsv3 must be 0, but not (" + opcode + ")");
            return null; //rsv1 rsv2 rsv3 must be 0     
        }
        //0x00 表示一个后续帧 
        //0x01 表示一个文本帧 
        //0x02 表示一个二进制帧 
        //0x03-07 为以后的非控制帧保留
        //0x8 表示一个连接关闭
        //0x9 表示一个ping
        //0xA 表示一个pong
        //0x0B-0F 为以后的控制帧保留
        final boolean control = (opcode & 0b0000_1000) != 0; //是否控制帧
        final byte crcode = buffer.get();  //第二个字节

        byte lengthCode = crcode;
        final boolean masked = (lengthCode & 0x80) == 0x80;
        if (masked) lengthCode ^= 0x80; //mask

        //判断Buffer剩余内容够不够基本信息的创建
        int minBufferLength = ((lengthCode <= 0x7D) ? 0 : (lengthCode == 0x7E ? 2 : 4)) + (masked ? 4 : 0);
        if (buffer.remaining() < minBufferLength) {
            byte[] bs = new byte[2 + buffer.remaining()];
            bs[0] = opcode;
            bs[1] = crcode;
            buffer.get(bs, 2, buffer.remaining());
            halfBytes.setValue(bs);
            return NONE;
        }

        int length;
        if (lengthCode <= 0x7D) { //125
            length = lengthCode;
        } else {
            if (control) {
                if (debug) logger.log(Level.FINE, " receive control command from websocket client");
                return null;
            }
            if (lengthCode == 0x7E) {//0x7E=126
                length = (int) buffer.getChar();
            } else {
                length = buffer.getInt();
            }
        }
        if (length > wsmaxbody && wsmaxbody > 0) {
            if (debug) logger.log(Level.FINE, "message length (" + length + ") too big, must less " + wsmaxbody + "");
            return null;
        }
        this.receiveLength = length;
        if (masked) {
            final byte[] masks = new byte[4];
            buffer.get(masks);
            this.receiveMasker = new ConvertMask() {

                private int index = 0;

                @Override
                public byte unmask(byte value) {
                    return (byte) (value ^ masks[index++ % 4]);
                }
            };
        }
        if (buffer.remaining() >= this.receiveLength) { //内容足够， 可以解析
            this.parseReceiveMessage(webSocket, buffer);
            this.receiveCount = this.receiveLength;
        } else {
            this.receiveCount = buffer.remaining();
            this.receiveBuffers = buffer.hasRemaining() ? new ByteBuffer[]{buffer} : null;
        }
        return this;
    }

    void parseReceiveMessage(WebSocket webSocket, ByteBuffer... buffers) {
        if (webSocket._engine.cryptor != null) {
            HttpContext context = webSocket._engine.context;
            buffers = webSocket._engine.cryptor.decrypt(buffers, context.getBufferSupplier(), context.getBufferConsumer());
        }
        if (this.type == FrameType.TEXT) {
            Convert textConvert = webSocket.getTextConvert();
            if (textConvert == null) {
                this.receiveMessage = new String(this.getReceiveBytes(buffers), StandardCharsets.UTF_8);
                this.receiveType = MessageType.STRING;
            } else {
                this.receiveMessage = textConvert.convertFrom(webSocket._messageTextType, this.receiveMasker, buffers);
                this.receiveCount = this.receiveLength;
                this.receiveType = MessageType.OBJECT;
            }
        } else if (this.type == FrameType.BINARY) {
            Convert binaryConvert = webSocket.getBinaryConvert();
            if (binaryConvert == null) {
                this.receiveMessage = this.getReceiveBytes(buffers);
                this.receiveType = MessageType.BYTES;
            } else {
                this.receiveMessage = binaryConvert.convertFrom(webSocket._messageTextType, this.receiveMasker, buffers);
                this.receiveCount = this.receiveLength;
                this.receiveType = MessageType.OBJECT;
            }
        } else if (this.type == FrameType.PING) {
            this.receiveMessage = this.getReceiveBytes(buffers);
            this.receiveType = MessageType.BYTES;
        } else if (this.type == FrameType.PONG) {
            this.receiveMessage = this.getReceiveBytes(buffers);
            this.receiveType = MessageType.BYTES;
        } else if (this.type == FrameType.CLOSE) {
            this.receiveMessage = this.getReceiveBytes(buffers);
            this.receiveType = MessageType.BYTES;
        }
    }

    boolean isReceiveFinished() {
        return this.receiveLength <= this.receiveCount;
    }

    byte[] getReceiveBytes(ByteBuffer... buffers) {
        final int length = this.receiveLength;
        if (length == 0) return new byte[0];
        byte[] bs = new byte[length];
        int index = 0;
        for (ByteBuffer buf : buffers) {
            int r = Math.min(buf.remaining(), length - index);
            buf.get(bs, index, r);
            index += r;
            if (index >= length) break;
        }
        this.receiveCount = index;
        ConvertMask mask = this.receiveMasker;
        if (mask != null) {
            for (int i = 0; i < bs.length; i++) {
                bs[i] = mask.unmask(bs[i]);
            }
        }
        return bs;
    }

}
