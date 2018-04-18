/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.ext;

import java.nio.channels.*;
import org.redkale.convert.*;

/**
 * java.nio.channels.CompletionHandler 的SimpledCoder实现, 只输出null
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 */
public final class CompletionHandlerSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, CompletionHandler> {

    public static final CompletionHandlerSimpledCoder instance = new CompletionHandlerSimpledCoder();

    @Override
    public void convertTo(W out, CompletionHandler value) {
        out.writeObjectNull(CompletionHandler.class);
    }

    @Override
    public CompletionHandler convertFrom(R in) {
        in.readObjectB(CompletionHandler.class);
        return null;
    }

}
