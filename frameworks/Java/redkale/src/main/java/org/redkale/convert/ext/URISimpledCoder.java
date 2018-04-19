/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.ext;

import java.net.*;
import org.redkale.convert.*;

/**
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 */
public class URISimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, URI> {

    public static final URLSimpledCoder instance = new URLSimpledCoder();

    @Override
    public void convertTo(final Writer out, final URI value) {
        if (value == null) {
            out.writeNull();
        } else {
            out.writeString(value.toString());
        }
    }

    @Override
    public URI convertFrom(Reader in) {
        final String str = in.readString();
        if (str == null) return null;
        try {
            return new URI(str);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
}
