/*
 * Copyright (c) 2018, org.smartboot. All rights reserved.
 * project name: smart-socket
 * file name: HttpV2Entity.java
 * Date: 2018-01-23
 * Author: sandao
 */

package org.smartboot.http;

import java.nio.ByteBuffer;

/**
 * Http消息体，兼容请求与响应
 *
 * @author 三刀 2018/06/02
 */
public class HttpEntityV2 {
    public final BufferRange verb = new BufferRange();
    public final BufferRange uri = new BufferRange();
    public final BufferRange protocol = new BufferRange();
    public final BufferRanges header = new BufferRanges();
    public int initPosition = 0;
    public State state = State.verb;
    public ByteBuffer buffer;
    private int currentPosition = 0;

    public void rest() {
        verb.reset();
        uri.reset();
        protocol.reset();
        header.reset();
        buffer = null;
        initPosition = 0;
        setCurrentPosition(0);
        state = State.verb;
    }

    public int getCurrentPosition() {
        return currentPosition;
    }

    public void setCurrentPosition(int currentPosition) {
        this.currentPosition = currentPosition;
    }

    public byte[] getBytes(BufferRange range) {
        int p = buffer.position();
        byte[] b = new byte[range.length];
        buffer.position(range.start);
        buffer.get(b);
        buffer.position(p);
        return b;
    }
}
