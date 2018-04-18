/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net;

import java.nio.ByteBuffer;
import java.util.function.*;

/**
 * 加密解密接口
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public interface Cryptor {

    /**
     * 加密
     *
     * @param buffers  待加密数据
     * @param supplier ByteBuffer生成器
     * @param consumer ByteBuffer回收器
     *
     * @return 加密后数据
     */
    public ByteBuffer[] encrypt(ByteBuffer[] buffers, final Supplier<ByteBuffer> supplier, final Consumer<ByteBuffer> consumer);

    /**
     * 解密
     *
     * @param buffers  待解密数据
     * @param supplier ByteBuffer生成器
     * @param consumer ByteBuffer回收器
     *
     * @return 解密后数据
     */
    public ByteBuffer[] decrypt(ByteBuffer[] buffers, final Supplier<ByteBuffer> supplier, final Consumer<ByteBuffer> consumer);
}
