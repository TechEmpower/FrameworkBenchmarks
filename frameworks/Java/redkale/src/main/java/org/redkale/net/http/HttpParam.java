/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 配合 &#64;HttpMapping 使用。
 * 用于对&#64;HttpMapping方法中参数描述
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Documented
@Target({METHOD})
@Retention(RUNTIME)
@Repeatable(HttpParam.HttpParams.class)
public @interface HttpParam {

    String name(); //参数名

    Class type(); //参数的数据类型

    String comment() default ""; //备注描述

    HttpParamSourceType src() default HttpParamSourceType.PARAMETER; //参数来源类型

    int radix() default 10; //转换数字byte/short/int/long时所用的进制数， 默认10进制

    boolean required() default true; //参数是否必传

    /**
     * 配合 &#64;HttpParam 使用。
     * 用于对&#64;HttpParam中参数的来源类型
     *
     * <p>
     * 详情见: https://redkale.org
     *
     * @author zhangjx
     */
    public static enum HttpParamSourceType {

        PARAMETER, HEADER, COOKIE;
    }

    @Documented
    @Target({METHOD})
    @Retention(RUNTIME)
    @interface HttpParams {

        HttpParam[] value();
    }

}
