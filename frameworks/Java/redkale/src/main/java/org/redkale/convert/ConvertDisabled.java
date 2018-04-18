/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 序列化时永久禁用该字段, 与ConvertColumn.ignore()的区别在于: ConvertDisabled不能通过ConvertEntity来解禁
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface ConvertDisabled {

    /**
     * 解析/序列化定制化的TYPE
     *
     * @return JSON or BSON or ALL
     */
    ConvertType type() default ConvertType.ALL;

    /**
     * ConvertDisabled 的多用类
     *
     * <p>
     * 详情见: https://redkale.org
     *
     * @author zhangjx
     */
    @Inherited
    @Documented
    @Target({METHOD, FIELD})
    @Retention(RUNTIME)
    public static @interface ConvertDisableds {

        ConvertDisabled[] value();
    }
}
