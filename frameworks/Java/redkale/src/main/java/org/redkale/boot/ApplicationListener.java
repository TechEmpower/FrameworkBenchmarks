/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.boot;

import org.redkale.util.AnyValue;

/**
 * Application启动和关闭时的监听事件 <br>
 * 只能通过application.xml配置
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public interface ApplicationListener {

    /**
     * 初始化方法
     *
     * @param config 配置参数
     */
    default void init(AnyValue config) {

    }

    /**
     * Application 在运行start前调用
     *
     * @param application Application
     */
    default void preStart(Application application) {
    }

    /**
     * Application 在运行shutdown前调用
     *
     * @param application Application
     */
    default void preShutdown(Application application) {
    }
}
