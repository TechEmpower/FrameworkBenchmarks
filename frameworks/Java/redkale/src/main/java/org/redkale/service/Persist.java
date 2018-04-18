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
 * Service类中临时缓存字段 <br>
 *
 * <b>注意: </b> 被标记字段的数据必须是可序列化和反序列化的, 且字段不能是static的， 如果字段类型不是Map或Collection类型则不能修饰为final
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Target({FIELD})
@Retention(RUNTIME)
public @interface Persist {

    /**
     * 临时缓存的超时秒数，超过指定秒数的缓存数据将会被废弃, 0表示不超时， 默认超时值为60秒
     *
     * @return int
     */
    int timeout() default 60;
}
