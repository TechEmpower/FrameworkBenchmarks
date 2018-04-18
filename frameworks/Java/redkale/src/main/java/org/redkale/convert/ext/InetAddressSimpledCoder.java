/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.ext;

import org.redkale.convert.SimpledCoder;
import org.redkale.convert.Writer;
import org.redkale.convert.Reader;
import java.net.*;

/**
 * InetAddress 的SimpledCoder实现
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 */
@SuppressWarnings("unchecked")
public final class InetAddressSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, InetAddress> {

    public static final InetAddressSimpledCoder instance = new InetAddressSimpledCoder();

    @Override
    public void convertTo(W out, InetAddress value) {
        if (value == null) {
            out.writeNull();
            return;
        }
        ByteArraySimpledCoder.instance.convertTo(out, value.getAddress());
    }

    @Override
    public InetAddress convertFrom(R in) {
        byte[] bytes = ByteArraySimpledCoder.instance.convertFrom(in);
        if (bytes == null) return null;
        try {
            return InetAddress.getByAddress(bytes);
        } catch (Exception ex) {
            return null;
        }
    }

    /**
     * InetSocketAddress 的SimpledCoder实现
     *
     * @param <R> Reader输入的子类型
     * @param <W> Writer输出的子类型
     */
    @SuppressWarnings("unchecked")
    public final static class InetSocketAddressSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, InetSocketAddress> {

        public static final InetSocketAddressSimpledCoder instance = new InetSocketAddressSimpledCoder();

        @Override
        public void convertTo(W out, InetSocketAddress value) {
            if (value == null) {
                out.writeNull();
                return;
            }
            ByteArraySimpledCoder.instance.convertTo(out, value.getAddress().getAddress());
            out.writeInt(value.getPort());
        }

        @Override
        public InetSocketAddress convertFrom(R in) {
            byte[] bytes = ByteArraySimpledCoder.instance.convertFrom(in);
            if (bytes == null) return null;
            int port = in.readInt();
            try {
                return new InetSocketAddress(InetAddress.getByAddress(bytes), port);
            } catch (Exception ex) {
                return null;
            }
        }

    }

    /**
     * InetAddress 的JsonSimpledCoder实现
     *
     * @param <R> Reader输入的子类型
     * @param <W> Writer输出的子类型
     */
    public final static class InetAddressJsonSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, InetAddress> {

        public static final InetAddressJsonSimpledCoder instance = new InetAddressJsonSimpledCoder();

        @Override
        public void convertTo(W out, InetAddress value) {
            if (value == null) {
                out.writeNull();
                return;
            }
            StringSimpledCoder.instance.convertTo(out, value.getHostAddress());
        }

        @Override
        public InetAddress convertFrom(R in) {
            String str = StringSimpledCoder.instance.convertFrom(in);
            if (str == null) return null;
            try {
                return InetAddress.getByName(str);
            } catch (Exception ex) {
                return null;
            }
        }

    }

    /**
     * InetSocketAddress 的JsonSimpledCoder实现
     *
     * @param <R> Reader输入的子类型
     * @param <W> Writer输出的子类型
     */
    public final static class InetSocketAddressJsonSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, InetSocketAddress> {

        public static final InetSocketAddressJsonSimpledCoder instance = new InetSocketAddressJsonSimpledCoder();

        @Override
        public void convertTo(W out, InetSocketAddress value) {
            if (value == null) {
                out.writeNull();
                return;
            }
            StringSimpledCoder.instance.convertTo(out, value.getHostString() + ":" + value.getPort());
        }

        @Override
        public InetSocketAddress convertFrom(R in) {
            String str = StringSimpledCoder.instance.convertFrom(in);
            if (str == null) return null;
            try {
                int pos = str.indexOf(':');
                return new InetSocketAddress(str.substring(0, pos), Integer.parseInt(str.substring(pos + 1)));
            } catch (Exception ex) {
                return null;
            }
        }

    }
}
