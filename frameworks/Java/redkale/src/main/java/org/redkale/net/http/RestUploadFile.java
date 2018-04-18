/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 *
 * 依附在RestService类的方法的参数上, 用于接收上传文件  <br>
 * 只能标记在byte[]/File/File[] 类型的参数上  <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Inherited
@Documented
@Target({PARAMETER, FIELD})
@Retention(RUNTIME)
public @interface RestUploadFile {

    /**
     * 可接收的文件大小最大值， 小于1表示无大小限制
     *
     * @return int
     */
    long maxLength() default 0;

    /**
     * 可接收的文件名正则表达式, 为空表示接收任何文件  <br>
     *
     * @return String
     */
    String fileNameReg() default "";

    /**
     * 可接收的ContentType正则表达式, 为空表示接收任何文件类型  <br>
     *
     * @return String
     */
    String contentTypeReg() default "";

    /**
     * 备注描述, 对应&#64;HttpParam.comment
     *
     * @return String
     */
    String comment() default "";
}
