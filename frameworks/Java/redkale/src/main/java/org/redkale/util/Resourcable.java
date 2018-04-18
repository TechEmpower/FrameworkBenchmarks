/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.util;

/**
 * 对象的类没有标记为&#64;Resource, 可以通过实现Resourcable接口实现动态获取Resource.name
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public interface Resourcable {

    public String resourceName();
}
