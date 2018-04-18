/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

/**
 * FilterValue主要用于复杂的表达式。<br>
 * 例如: col / 10 = 3 、MOD(col, 8) &gt; 0 这些都不是单独一个数值能表达的，因此需要FilterValue 才构建 8 、 &gt; 、0 组合值。
 *
 * <p>
 * 详情见: https://redkale.org
 * 
 * @author zhangjx
 */
public class FilterValue implements java.io.Serializable {

    private Number optvalue;

    private FilterExpress express;

    private Number destvalue;

    public FilterValue() {
    }

    public FilterValue(Number optvalue, Number destvalue) {
        this(optvalue, FilterExpress.EQUAL, destvalue);
    }

    public FilterValue(Number optvalue, FilterExpress express) {
        this(optvalue, express, 0);
    }

    public FilterValue(Number optvalue, FilterExpress express, Number destvalue) {
        this.optvalue = optvalue;
        this.express = express;
        this.destvalue = destvalue;
    }

    public Number getOptvalue() {
        return optvalue == null ? 0 : optvalue;
    }

    public void setOptvalue(Number optvalue) {
        this.optvalue = optvalue;
    }

    public FilterExpress getExpress() {
        return express == null ? FilterExpress.EQUAL : express;
    }

    public void setExpress(FilterExpress express) {
        this.express = express;
    }

    public Number getDestvalue() {
        return destvalue == null ? 0 : destvalue;
    }

    public void setDestvalue(Number destvalue) {
        this.destvalue = destvalue;
    }

    @Override
    public String toString() {
        return FilterValue.class.getSimpleName() + "[optvalue=" + getOptvalue() + ", express=" + getExpress() + ", destvalue=" + getDestvalue() + "]";
    }
}
