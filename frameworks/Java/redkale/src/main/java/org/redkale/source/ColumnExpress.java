/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

/**
 * 函数表达式， 均与SQL定义中的表达式相同
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public enum ColumnExpress {
    /**
     * 直接赋值 col = val
     */
    MOV,
    /**
     * 追加值 col = col + val
     */
    INC,
    /**
     * 乘值 col = col * val
     */
    MUL,
    /**
     * 与值 col = col &#38; val
     */
    AND, //与值 col = col & val
    /**
     * 或值 col = col | val
     */
    ORR;
}
