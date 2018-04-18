/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.ext;

import java.io.File;
import org.redkale.convert.*;

/**
 * 文件 的SimpledCoder实现
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类型
 * @param <W> Writer输出的子类型
 */
public class FileSimpledCoder<R extends Reader, W extends Writer> extends SimpledCoder<R, W, File> {

    public static final PatternSimpledCoder instance = new PatternSimpledCoder();

    @Override
    public void convertTo(W out, File value) {
        if (value == null) {
            out.writeNull();
        } else {
            out.writeString(value.getPath());
        }
    }

    @Override
    public File convertFrom(R in) {
        String value = in.readString();
        if (value == null) return null;
        return new File(value);
    }

}
