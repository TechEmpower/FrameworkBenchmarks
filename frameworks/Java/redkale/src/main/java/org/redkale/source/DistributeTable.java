/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * Entity分库分表的注解，需要结合DistributeTableStrategy使用 <br>
 * 标记为 &#64;DistributeTable的Entity类视为需要进行分库分表操作   <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Target({TYPE})
@Retention(RUNTIME)
public @interface DistributeTable {

    Class<? extends DistributeTableStrategy> strategy();
}
