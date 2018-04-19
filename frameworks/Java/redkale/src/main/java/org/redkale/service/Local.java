/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.service;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 本地模式注解。<br>
 * 声明为Local的Service只能以本地模式存在， 即使配置文件中配置成远程模式也将被忽略。 <br>
 * Service里被标记为Local的public方法不会被重载。
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Inherited
@Documented
@Target({TYPE, METHOD, PARAMETER})
@Retention(RUNTIME)
public @interface Local {

    String comment() default ""; //备注描述
}
