/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.util.Arrays;

/**
 * FilterFuncColumn用于getNumberMap获取列表似数据, getNumberResult获取单字段值， getNumberMap获取多字段值
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class FilterFuncColumn implements java.io.Serializable {

    public static final String COLUMN_NULL = "*";

    FilterFunc func;

    String[] columns; //为null，将使用*代替

    Number defvalue;

    public FilterFuncColumn() {
    }

    public static FilterFuncColumn create(final FilterFunc func) {
        return new FilterFuncColumn(func);
    }

    public static FilterFuncColumn create(final FilterFunc func, final String... columns) {
        return new FilterFuncColumn(func, columns);
    }

    public static FilterFuncColumn create(final FilterFunc func, final Number defvalue, final String... columns) {
        return new FilterFuncColumn(func, defvalue, columns);
    }

    String[] cols() {
        return columns == null || columns.length == 0 ? new String[]{COLUMN_NULL} : columns;
    }

    String col(String column) {
        return column == null || column.isEmpty() ? COLUMN_NULL : column;
    }

    public FilterFuncColumn(final FilterFunc func) {
        this(func, (Number) null);
    }

    public FilterFuncColumn(final FilterFunc func, final String... columns) {
        this(func, null, columns);
    }

    public FilterFuncColumn(final FilterFunc func, final Number defvalue, final String... columns) {
        this.func = func;
        this.defvalue = defvalue;
        this.columns = columns;
    }

    public FilterFunc getFunc() {
        return func;
    }

    public void setFunc(FilterFunc func) {
        this.func = func;
    }

    public String[] getColumns() {
        return columns;
    }

    public void setColumns(String[] columns) {
        this.columns = columns;
    }

    public Number getDefvalue() {
        return defvalue;
    }

    public void setDefvalue(Number defvalue) {
        this.defvalue = defvalue;
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName() + "{func:" + this.func + ", columns:" + Arrays.toString(this.columns) + ", defvalue:" + this.defvalue + "}";
    }
}
