/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.PARAMETER;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 *
 * 依附在RestService类的方法的参数上  <br>
 * name='&#38;' 表示当前用户  <br>
 * name='#'表示截取uri最后一段  <br>
 * name='#xxx:'表示从uri中/pipes/xxx:v/截取xxx:的值  <br>
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Inherited
@Documented
@Target({PARAMETER})
@Retention(RUNTIME)
public @interface RestParam {

    //name='&'表示当前用户;
    /**
     * 参数名   <br>
     * name='&#38;'表示当前用户;  <br>
     * name='#'表示截取uri最后一段;  <br>
     * name='#xxx:'表示从uri中/pipes/xxx:v/截取xxx:的值  <br>
     * 若方法名以find、delete开头且方法的参数只有一个且参数类型是基本数据类型或String，则默认值为"#"  <br>
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
     * 参数是否必传, 仅供apidoc功能使用，框架运行中不作验证
     *
     * @return boolean
     */
    boolean required() default true;

    /**
     * 备注描述
     *
     * @return String
     */
    String comment() default "";
}
