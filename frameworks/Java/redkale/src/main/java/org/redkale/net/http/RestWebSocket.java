/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import org.redkale.net.Cryptor;

/**
 * 只能依附在WebSocket类上，name默认为Service的类名小写并去掉Service字样及后面的字符串 (如HelloWebSocket/HelloWebSocketImpl，的默认路径为 hello)。 <br>
 * <b>注意: </b> 被标记&#64;RestWebSocket的WebSocket不能被修饰为abstract或final，且其内部标记为&#64;Resource的字段只能是protected或public，且必须要有一个protected或public的空参数构造函数。 <br>
 * name值支持含{system.property.xxx}模式
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@Inherited
@Documented
@Target({TYPE})
@Retention(RUNTIME)
public @interface RestWebSocket {

    /**
     * 模块名, 只能是模块名,不能含特殊字符， 只能小写字母+数字，且不能以数字开头
     *
     * @return 模块名
     */
    String name() default "";

    /**
     * 目录名, 不能含特殊字符， 只能小写字母+数字，且不能以数字开头
     *
     * @return 目录名
     */
    String catalog() default "";

    /**
     * 是否为二进制消息, 默认为文本消息
     *
     * @return boolean
     */
    boolean binary() default false;

    /**
     * 是否单用户单连接, 默认单用户单连接
     *
     * @return 是否单用户单连接
     */
    boolean single() default true;

    /**
     * WebSocket.createUserid返回的值是否不能表示用户登录态， 比如createUserid返回随机的UUID那么anyuser应该为true
     *
     * @return 默认false
     */
    boolean anyuser() default false;

    /**
     * WebScoket服务器给客户端进行ping操作的间隔时间, 单位: 秒， 默认值：15秒
     *
     * @return int
     */
    int liveinterval() default WebSocketServlet.DEFAILT_LIVEINTERVAL;

    /**
     * 加密解密器
     *
     * @return Cryptor
     */
    Class<? extends Cryptor> cryptor() default Cryptor.class;

    /**
     * 最大连接数, 小于1表示无限制
     *
     * @return 最大连接数
     */
    int wsmaxconns() default 0;

    /**
     * 最大消息体长度, 小于1表示无限制
     *
     * @return 最大消息体长度
     */
    int wsmaxbody() default 32 * 1024;

    /**
     * 是否屏蔽该类的转换
     *
     * @return 默认false
     */
    boolean ignore() default false;

    /**
     * 同&#64;WebServlet的repair属性
     *
     * @return 默认true
     */
    boolean repair() default true;

    /**
     * 备注描述
     *
     * @return 备注描述
     */
    String comment() default "";
}
