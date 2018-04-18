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
 * 标记在RestWebSocket的接收消息方法上; <br>
 * 注意：被标记的方法必须同时符合以下条件: <br>
 * 1、必须修饰为public
 * 2、不能修饰为final和static
 * 3、返回值必须是void
 * 4、不能throws检查型异常
 *
 * <br><p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Inherited
@Documented
@Target({METHOD})
@Retention(RUNTIME)
public @interface RestOnMessage {

    /**
     * 请求的方法名, 不能含特殊字符,不能以数字开头(能作为变量名)
     *
     * @return String
     */
    String name();

    /**
     * 备注描述
     *
     * @return String
     */
    String comment() default "";
}
