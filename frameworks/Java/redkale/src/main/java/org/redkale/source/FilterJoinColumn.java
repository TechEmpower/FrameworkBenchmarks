/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * 关联表过滤条件 <br>
 * 关联关系表必须含主表， 不能是主表A关联表B，表B再关联表C，只能是主表A关联表B，主表A关联表C <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Inherited
@Documented
@Target({FIELD})
@Retention(RUNTIME)
public @interface FilterJoinColumn {

    /**
     * 关联表 通常join表默认别名为b/c/d/...自增， 被join表默认别名为a
     *
     * @return 关联表
     */
    Class table();

    /**
     *
     * 多个关联字段, 默认使用join表(b)的主键, join表与被join表(a)的字段必须一样   <br>
     * 例如: SELECT a.* FROM user a INNER JOIN record b ON a.userid = b.userid AND a.usertype = b.usertype   <br>
     * 那么注解为: &#64;FilterJoinColumn(table = Record.class, columns = {"userid", "usertype"})   <br>
     * <p>
     * columns中的字段名如果不一致，可以将两个字段名用=连接成一个字段名   <br>
     * 例如: SELECT a.* FROM user a INNER JOIN record b ON a.userid = b.buyerid AND a.usertype = b.usertype   <br>
     * 那么注解为: &#64;FilterJoinColumn(table = Record.class, columns = {"userid=buyerid", "usertype"})   <br>
     *
     * @return 关联字段
     */
    String[] columns();

    /**
     * 备注描述
     *
     * @return 备注描述
     */
    String comment() default "";
}
