/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;

/**
 * 只能依附在Service实现类的public方法上，且方法如果throws只能是IOException  <br>
 * value默认为"/" + Service的类名去掉Service字样的小写字符串 (如HelloService，的默认路径为/hello)。  <br>
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Inherited
@Documented
@Target({METHOD})
@Retention(RUNTIME)
@Repeatable(RestMapping.RestMappings.class)
public @interface RestMapping {

    /**
     * 是否屏蔽该方法进行HttpMapping转换
     *
     * @return boolean
     */
    boolean ignore() default false;

    /**
     * 请求的方法名, 不能含特殊字符
     * 默认为方法名的小写(若方法名以createXXX、updateXXX、deleteXXX、queryXXX、findXXX、existsXXX且XXXService为Service的类名将只截取XXX之前)
     *
     * @return String
     */
    String name() default "";

    /**
     * 备注描述, 对应&#64;HttpMapping.comment
     *
     * @return String
     */
    String comment() default "";

    /**
     * 是否鉴权，默认需要鉴权, 对应&#64;HttpMapping.auth
     *
     * @return boolean
     */
    boolean auth() default true;

    /**
     * 操作ID值，鉴权时用到, 对应&#64;HttpMapping.actionid
     *
     * @return int
     */
    int actionid() default 0;

    /**
     * 结果缓存的秒数, 为0表示不缓存, 对应&#64;HttpMapping.cacheseconds
     *
     * @return int
     */
    int cacheseconds() default 0;

    /**
     * 允许方法(不区分大小写),如:GET/POST/PUT,为空表示允许所有方法, 对应&#64;HttpMapping.methods
     *
     * @return String[]
     */
    String[] methods() default {};

    @Inherited
    @Documented
    @Target({METHOD})
    @Retention(RUNTIME)
    @interface RestMappings {

        RestMapping[] value();
    }
}
