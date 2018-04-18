/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

/**
 * 常见的SQL聚合函数
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public enum FilterFunc {
    AVG, //平均值
    COUNT, //总数
    DISTINCTCOUNT, //去重总数
    MAX, //最大值
    MIN, //最小值
    SUM; //求和

    public String getColumn(String col) {
        if (this == DISTINCTCOUNT) return "COUNT(DISTINCT " + col + ")";
        return this.name() + "(" + col + ")";
    }
}
