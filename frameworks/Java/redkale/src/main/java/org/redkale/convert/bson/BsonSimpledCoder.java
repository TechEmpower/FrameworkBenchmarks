/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert.bson;

import org.redkale.convert.SimpledCoder;

/**
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> 序列化/反解析的数据类型
 */
public abstract class BsonSimpledCoder<T> extends SimpledCoder<BsonReader, BsonWriter, T> {

}
