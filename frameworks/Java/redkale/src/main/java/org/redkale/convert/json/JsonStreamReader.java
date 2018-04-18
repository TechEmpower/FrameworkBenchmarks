/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.json;

import java.io.*;
import org.redkale.convert.*;

/**
 *
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
class JsonStreamReader extends JsonByteBufferReader {

    private InputStream in;

    protected JsonStreamReader(InputStream in) {
        super((ConvertMask) null);
        this.in = in;
    }

    @Override
    protected boolean recycle() {
        super.recycle();   // this.position 初始化值为-1
        this.in = null;
        return false;
    }

    @Override
    protected byte nextByte() {
        try {
            byte b = (byte) in.read();
            this.position++;
            return b;
        } catch (IOException e) {
            throw new ConvertException(e);
        }
    }
}
