/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

/**
 * 序列化类型枚举，结合&#64;ConvertColumn使用
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public enum ConvertType {

    JSON(1),
    BSON(2),
    DIY(64),
    ALL(127);

    private final int value;

    private ConvertType(int v) {
        this.value = v;
    }

    public boolean contains(ConvertType type) {
        if (type == null) return false;
        return this.value >= type.value && (this.value & type.value) > 0;
    }
}
