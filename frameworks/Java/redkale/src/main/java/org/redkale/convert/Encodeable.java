/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.reflect.Type;

/**
 * 序列化操作类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <W> Writer输出的子类
 * @param <T> 序列化的数据类型
 */
public interface Encodeable<W extends Writer, T> {

    public void convertTo(final W out, T value);

    /**
     * 泛型映射接口
     *
     * @return 返回序列化对象类的数据类型
     */
    public Type getType();

}
