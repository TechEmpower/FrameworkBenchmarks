/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.service;

import org.redkale.util.*;

/**
 * 所有Service的实现类不得声明为final， 允许远程模式的public方法都不能声明为final。<br>
 * 注意: "$"是一个很特殊的Service.name值 。 被标记为&#64;Resource(name = "$") 的Service的资源名与所属父Service的资源名一致。<br>
 *
 * <blockquote><pre>
 * Service的资源类型
 * 业务逻辑的Service通常有两种编写方式：
 *    1、只写一个Service实现类。
 *    2、先定义业务的Service接口或抽象类，再编写具体实现类。
 * 第二种方式需要在具体实现类上使用&#64;ResourceType指明资源注入的类型。
 * </pre></blockquote>
 *
 * <blockquote><pre>
 * 异步方法：
 * Service编写异步方法：
 *    1、异步方法有且仅有一个类型为CompletionHandler的参数， 返回类型必须是void。若参数类型为CompletionHandler子类，必须保证其子类可被继承且completed、failed可被重载且包含空参数的构造函数。
 *    2、异步方法返回类型是CompletableFuture。
 * 例如:
 *      public void insertRecord(CompletionHandler&#60;Integer, Record&#62; handler, String name, &#64;RpcAttachment Record record);
 *
 * </pre></blockquote>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public interface Service {

    /**
     * 该方法必须是可以重复调用， 当reload时需要重复调用init方法
     * 远程模式下该方法会重载成空方法
     *
     * @param config 配置参数
     */
    default void init(AnyValue config) {

    }

    /**
     * 进程退出时，调用Service销毁
     * 远程模式下该方法会重载成空方法
     *
     * @param config 配置参数
     */
    default void destroy(AnyValue config) {

    }

}
