/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.*;
import java.util.*;
import java.util.function.*;

/**
 * VirtualEntity表示虚拟的数据实体类， 通常Entity都会映射到数据库中的某个表，而标记为&#64;VirtualEntity的Entity类只存在EntityCache中
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Documented
@Target(TYPE)
@Retention(RUNTIME)
public @interface VirtualEntity {

    /**
     * DataSource是否直接返回对象的真实引用， 而不是copy一份
     *
     * @return boolean
     */
    boolean direct() default false;

    /**
     * 初始化时数据的加载器
     *
     * @return Class
     */
    Class<? extends BiFunction<DataSource, Class, List>> loader() default DefaultFunctionLoader.class;

    /**
     * 默认全量加载器
     *
     */
    public static class DefaultFunctionLoader implements BiFunction<DataSource, Class, List> {

        @Override
        public List apply(DataSource source, Class type) {
            return null;
        }
    }
}
