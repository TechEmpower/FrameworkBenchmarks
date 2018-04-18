/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.util;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.*;

/**
 * 显式的指明资源类型。
 * 调用ResourceFactory.register(Object rs)时通常执行的是ResourceFactory.register(rs.getClass(), Object rs);
 * 若rs.getClass()的类标记了&#64;ResourceType, 则使用&#64;ResourceType.value()的class值进行注入。
 * 
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Inherited
@Documented
@Target({TYPE})
@Retention(RUNTIME)
public @interface ResourceType {

    Class value();
}
