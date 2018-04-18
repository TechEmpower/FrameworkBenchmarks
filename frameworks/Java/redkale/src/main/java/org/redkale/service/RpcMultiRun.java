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
 * RpcMultiRun 只对本地模式Service有效
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
public @interface RpcMultiRun {

    boolean selfrun() default true; //当前本地实例是否运行指定操作；只有当指定操作的方法的返回值为void时，该值才能为true，否则忽略。

    boolean samerun() default true;  //是否同组节点运行指定操作

    boolean diffrun() default true; //是否不同组节点运行指定操作

    boolean async() default true; //分布式运行是否采用异步模式
}
