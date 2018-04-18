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
 * 配合 HttpServlet 使用。
 * 用于对&#64;WebServlet对应的url进行细分。 其url必须是包含WebServlet中定义的前缀， 且不能是正则表达式
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Documented
@Target({METHOD})
@Retention(RUNTIME)
public @interface HttpMapping {

    /**
     * 操作ID值，鉴权时用到
     *
     * @return int
     */
    int actionid() default 0;

    String url();

    /**
     * 结果缓存的秒数, 为0表示不缓存 <br>
     *  * 当值大于0，将被缓存一段时间(默认值 seconds=15秒)。 <br>
     * 通常情况下需要 auth() == true 才使用，没有标记auth==true方法一般输出的结果与当前用户信息有关。 <br>
     *
     * @return int
     */
    int cacheseconds() default 0;

    /**
     * 是否鉴权，默认需要鉴权 <br>
     *
     * @return boolean
     */
    boolean auth() default true;

    /**
     * 允许方法(不区分大小写),如:GET/POST/PUT,为空表示允许所有方法
     *
     * @return String[]
     */
    String[] methods() default {};

    /**
     * 是否能被继承, 当 HttpServlet 被继承后该方法是否能被子类继承
     *
     * @return boolean
     */
    boolean inherited() default true;

    /**
     * 输出结果的数据类型
     *
     * @return String
     */
    String result() default "Object";

    /**
     * 输出结果的数据类型集合，由于结果类型可能是泛型而注解的参数值不支持泛型，因此加入明细数据类型集合
     *
     * @return Class[]
     */
    Class[] results() default {};

    /**
     * 备注描述
     *
     * @return String
     */
    String comment() default "";
}
