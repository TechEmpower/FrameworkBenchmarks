/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.sncp;

import java.net.*;
import java.nio.*;
import java.util.logging.Level;
import org.redkale.convert.bson.*;
import org.redkale.net.*;
import org.redkale.util.*;

/**
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public final class SncpRequest extends Request<SncpContext> {

    public static final int HEADER_SIZE = 60;

    public static final byte[] DEFAULT_HEADER = new byte[HEADER_SIZE];

    protected final BsonConvert convert;

    private long seqid;

    private int serviceversion;

    private DLong serviceid;

    private DLong actionid;

    private int bodylength;

    private int bodyoffset;

    private boolean ping;

    private byte[] body;

    private byte[] bufferbytes = new byte[6];

    protected SncpRequest(SncpContext context) {
        super(context);
        this.convert = context.getBsonConvert();
    }

    @Override
    protected int readHeader(ByteBuffer buffer) {
        if (buffer.remaining() < HEADER_SIZE) {
            this.ping = true;
            return 0;
        }
        //---------------------head----------------------------------
        this.seqid = buffer.getLong();
        if (buffer.getChar() != HEADER_SIZE) {
            if (context.getLogger().isLoggable(Level.FINEST)) context.getLogger().finest("sncp buffer header.length not " + HEADER_SIZE);
            return -1;
        }
        this.serviceid = DLong.read(buffer);
        this.serviceversion = buffer.getInt();
        this.actionid = DLong.read(buffer);
        buffer.get(bufferbytes);
        this.bodylength = buffer.getInt();

        if (buffer.getInt() != 0) {
            if (context.getLogger().isLoggable(Level.FINEST)) context.getLogger().finest("sncp buffer header.retcode not 0");
            return -1;
        }
        //---------------------body----------------------------------
        this.body = new byte[this.bodylength];
        int len = Math.min(this.bodylength, buffer.remaining());
        buffer.get(body, 0, len);
        this.bodyoffset = len;
        return bodylength - len;
    }

    @Override
    protected int readBody(ByteBuffer buffer) {
        final int framelen = buffer.remaining();
        buffer.get(this.body, this.bodyoffset, framelen);
        this.bodyoffset += framelen;
        return framelen;
    }

    @Override
    protected void prepare() {
        this.keepAlive = true;
    }

    @Override
    public String toString() {
        return SncpRequest.class.getSimpleName() + "{seqid=" + this.seqid
            + ",serviceversion=" + this.serviceversion + ",serviceid=" + this.serviceid
            + ",actionid=" + this.actionid + ",bodylength=" + this.bodylength
            + ",bodyoffset=" + this.bodyoffset + ",remoteAddress=" + getRemoteAddress() + "}";
    }

    @Override
    protected void recycle() {
        this.seqid = 0;
        this.serviceid = null;
        this.serviceversion = 0;
        this.actionid = null;
        this.bodylength = 0;
        this.bodyoffset = 0;
        this.body = null;
        this.ping = false;
        this.bufferbytes[0] = 0;
        super.recycle();
    }

    protected boolean isPing() {
        return ping;
    }

    public byte[] getBody() {
        return body;
    }

    public long getSeqid() {
        return seqid;
    }

    public int getServiceversion() {
        return serviceversion;
    }

    public DLong getServiceid() {
        return serviceid;
    }

    public DLong getActionid() {
        return actionid;
    }

    public InetSocketAddress getRemoteAddress() {
        if (bufferbytes[0] == 0) return null;
        return new InetSocketAddress((0xff & bufferbytes[0]) + "." + (0xff & bufferbytes[1]) + "." + (0xff & bufferbytes[2]) + "." + (0xff & bufferbytes[3]),
            ((0xff00 & (bufferbytes[4] << 8)) | (0xff & bufferbytes[5])));
    }

}
