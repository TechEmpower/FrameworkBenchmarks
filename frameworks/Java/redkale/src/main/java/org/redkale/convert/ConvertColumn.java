/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;

/**
 * 依附在setter、getter方法、字段进行简单的配置
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
@Repeatable(ConvertColumn.ConvertColumns.class)
public @interface ConvertColumn {

    /**
     * 给字段取个别名
     *
     * @return 字段别名
     */
    String name() default "";

    /**
     * 给字段取个序号ID，值小靠前
     *
     * @return 字段排序ID
     */
    int index() default 0;

    /**
     * 解析/序列化时是否屏蔽该字段
     *
     * @return 是否屏蔽该字段
     */
    boolean ignore() default false;

    /**
     * 解析/序列化定制化的TYPE
     *
     * @return JSON or BSON or ALL
     */
    ConvertType type() default ConvertType.ALL;

    /**
     * ConvertColumn 的多用类
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
    public static @interface ConvertColumns {

        ConvertColumn[] value();
    }
}
