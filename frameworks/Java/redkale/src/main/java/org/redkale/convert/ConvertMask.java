/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

/**
 * Mask接口
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public interface ConvertMask {

    default byte mask(byte value) {
        return value;
    }

    default byte unmask(byte value) {
        return value;
    }
}
