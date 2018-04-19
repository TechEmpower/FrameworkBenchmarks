/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.service;

import java.lang.annotation.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import org.redkale.util.*;

/**
 * 参数回写, 当Service的方法需要更改参数对象内部的数据时，需要使用RpcCall
 *
 * <p> 详情见: https://redkale.org
 * @author zhangjx
 */
@Inherited
@Documented
@Target({ElementType.PARAMETER})
@Retention(RUNTIME)
public @interface RpcCall {

    Class<? extends Attribute> value();
}
