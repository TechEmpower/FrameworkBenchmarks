/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.util;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * &#64;Resource资源被更新时的监听事件。本注解只能标记在方法参数为(String name, T newVal, T oldVal)上。
 * 方法在资源被更新以后调用。
 *
 * <blockquote><pre>
 * public class RecordService implements Service {
 *
 *    &#64;Resource(name = "record.id")
 *    private int id;
 *
 *    &#64;Resource(name = "record.name")
 *    private String name;
 *
 *    &#64;ResourceListener
 *    private void changeResource(String name, Object newVal, Object oldVal) {
 *        System.out.println("@Resource = " + name + " 资源变更:  newVal = " + newVal + ", oldVal = " + oldVal);
 *    }
 *
 *    public static void main(String[] args) throws Exception {
 *        ResourceFactory factory = ResourceFactory.root();
 *        factory.register("record.id", "2345"); 
 *        factory.register("record.name", "my old name"); 
 *        Record record = new Record();
 *        factory.inject(record);
 *        factory.register("record.name", "my new name"); 
 *   }
 * 
 * }
 * </pre></blockquote>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Documented
@Target({METHOD})
@Retention(RUNTIME)
public @interface ResourceListener {

}
