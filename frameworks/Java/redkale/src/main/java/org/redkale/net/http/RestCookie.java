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
 * 只能注解于RestService类的方法的String参数或参数内的String字段
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Inherited
@Documented
@Target({PARAMETER, FIELD})
@Retention(RUNTIME)
public @interface RestCookie {

    /**
     * cookie名
     *
     * @return String
     */
    String name();

    /**
     * 转换数字byte/short/int/long时所用的进制数， 默认10进制
     *
     * @return int
     */
    int radix() default 10;

    /**
     * 备注描述
     *
     * @return String
     */
    String comment() default "";
}
