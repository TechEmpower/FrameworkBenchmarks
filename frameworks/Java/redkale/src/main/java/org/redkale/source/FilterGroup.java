/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import static java.lang.annotation.RetentionPolicy.RUNTIME;
import static java.lang.annotation.ElementType.FIELD;
import java.lang.annotation.*;

/**
 * 默认情况下FilterBean下的过滤字段之间是AND关系。  <br>
 * 当需要使用OR或AND OR组合过滤查询时需要使用 FilterGroup。  <br>
 * FilterGroup 的value 必须是[OR]或者[AND]开头， 多级需要用点.分隔。 (注: 暂时不支持多级)  <br>
 * 示例一:
 * <blockquote><pre>
 * public class TestFilterBean implements FilterBean {
 *
 *      private int id;
 *
 *      &#64;FilterGroup("[OR]g1")
 *      private String desc;
 *
 *      &#64;FilterGroup("[OR]g1")
 *      private String name;
 * }
 * </pre></blockquote>
 * 转换的SQL语句为: WHERE id = ? AND (desc = ? OR name = ?)
 *
 * 示例二:
 * <blockquote><pre>
 * public class TestFilterBean implements FilterBean {
 *
 *      private int id;
 *
 *      &#64;FilterGroup("[OR]g1.[AND]subg1")
 *      &#64;FilterColumn(express = LIKE)
 *      private String desc;
 *
 *      &#64;FilterGroup("[OR]g1.[AND]subg1")
 *      &#64;FilterColumn(express = LIKE)
 *      private String name;
 *
 *      &#64;FilterGroup("[OR]g1.[OR]subg2")
 *      private int age;
 *
 *      &#64;FilterGroup("[OR]g1.[OR]subg2")
 *      private int birthday;
 * }
 * </pre></blockquote>
 * 转换的SQL语句为: WHERE id = ? AND ((desc LIKE ? AND name LIKE ?) OR (age = ? OR birthday = ?))  <br>
 * 因为默认是AND关系， &#64;FilterGroup("") 等价于 &#64;FilterGroup("[AND]")  <br>
 * 所以示例二的&#64;FilterGroup("[OR]g1.[AND]subg1") 可以简化为 &#64;FilterGroup("[OR]g1.subg1")  <br>
 */
/**
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Inherited
@Documented
@Target({FIELD})
@Retention(RUNTIME)
public @interface FilterGroup {

    String value() default "[AND]";
}
