/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.service;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.reflect.*;
import java.util.*;

/**
 * 用于定义错误码的注解  <br>
 * 结果码定义范围:  <br>
 *    // 10000001 - 19999999 预留给Redkale的核心包使用  <br>
 *    // 20000001 - 29999999 预留给Redkale的扩展包使用  <br>
 *    // 30000001 - 99999999 预留给Dev开发系统自身使用  <br>
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Inherited
@Documented
@Target({FIELD})
@Retention(RUNTIME)
@Repeatable(RetLabel.RetLabels.class)
public @interface RetLabel {

    String value();

    String locale() default "";

    @Inherited
    @Documented
    @Target({FIELD})
    @Retention(RUNTIME)
    @interface RetLabels {

        RetLabel[] value();
    }

    public static abstract class RetLoader {

        public static Map<String, Map<Integer, String>> loadMap(Class clazz) {
            final Map<String, Map<Integer, String>> rets = new HashMap<>();
            for (Field field : clazz.getFields()) {
                if (!Modifier.isStatic(field.getModifiers())) continue;
                if (field.getType() != int.class) continue;
                RetLabel info = field.getAnnotation(RetLabel.class);
                if (info == null) continue;
                int value;
                try {
                    value = field.getInt(null);
                } catch (Exception ex) {
                    ex.printStackTrace();
                    continue;
                }
                rets.computeIfAbsent(info.locale(), (k) -> new HashMap<>()).put(value, info.value());
            }
            return rets;
        }

        @Deprecated
        public static Map<Integer, String> load(Class clazz) {
            return loadMap(clazz).computeIfAbsent("", (k) -> new HashMap<>());
        }
    }
}
