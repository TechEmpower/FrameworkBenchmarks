/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

/**
 * 序列化自定义异常类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class ConvertException extends RuntimeException {

    public ConvertException() {
        super();
    }

    public ConvertException(String s) {
        super(s);
    }

    public ConvertException(String message, Throwable cause) {
        super(message, cause);
    }

    public ConvertException(Throwable cause) {
        super(cause);
    }
}
