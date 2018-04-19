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
 * 只能依附在Service实现类的public方法上, 当方法的返回值以JSON输出时对指定类型的转换设定。  <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Inherited
@Documented
@Target({METHOD})
@Retention(RUNTIME)
@Repeatable(RestConvert.RestConverts.class)
public @interface RestConvert {

    boolean tiny() default true;

    boolean skipIgnore() default false;

    Class type();

    String[] ignoreColumns() default {};

    String[] convertColumns() default {};

    @Inherited
    @Documented
    @Target({METHOD})
    @Retention(RUNTIME)
    @interface RestConverts {

        RestConvert[] value();
    }
}
