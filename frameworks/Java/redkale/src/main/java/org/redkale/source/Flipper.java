/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.io.Serializable;
import org.redkale.util.Comment;

/**
 * 翻页对象, offset从0开始, limit必须大于0
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public final class Flipper implements Serializable, Cloneable {

    public static int DEFAULT_LIMIT = 20;

    @Comment("每页多少行")
    private int limit = DEFAULT_LIMIT;

    @Comment("记录行的偏移量，从0开始")
    private int offset = 0;

    @Comment("排序字段, 可多字段排序")
    private String sort = "";

    public Flipper() {
    }

    public Flipper(int limit) {
        this.limit = limit > 0 ? limit : 0;
    }

    public Flipper(String sortColumn) {
        this.sort = sortColumn;
    }

    public Flipper(int limit, int offset) {
        this.limit = limit > 0 ? limit : 0;
        this.offset = offset < 0 ? 0 : offset;
    }

    public Flipper(int limit, String sortColumn) {
        this.limit = limit > 0 ? limit : 0;
        this.sort = sortColumn;
    }

    public Flipper(int limit, int offset, String sortColumn) {
        this.limit = limit > 0 ? limit : 0;
        this.offset = offset < 0 ? 0 : offset;
        this.sort = sortColumn;
    }

    public Flipper copyTo(Flipper copy) {
        if (copy == null) return copy;
        copy.offset = this.offset;
        copy.limit = this.limit;
        copy.sort = this.sort;
        return copy;
    }

    public Flipper copyFrom(Flipper copy) {
        if (copy == null) return this;
        this.offset = copy.offset;
        this.limit = copy.limit;
        this.sort = copy.sort;
        return this;
    }

    public Flipper next() {
        this.offset = getOffset() + this.limit;
        return this;
    }

    @Override
    @SuppressWarnings("CloneDoesntCallSuperClone")
    public Flipper clone() {
        return this.copyTo(new Flipper());
    }

    @Override
    public String toString() {
        return this.getClass().getSimpleName() + "{offset:" + this.offset + ", limit:" + this.limit + ", sort:" + this.sort + "}";
    }

    public int getLimit() {
        return limit;
    }

    public void setLimit(int limit) {
        this.limit = limit;
    }

    public Flipper limit(int limit) {
        setLimit(limit);
        return this;
    }

    public Flipper unlimit() {
        this.limit = 0;
        return this;
    }

    public int getOffset() {
        return offset;
    }

    public void setOffset(int offset) {
        this.offset = offset < 0 ? 0 : offset;
    }

    public Flipper offset(int offset) {
        setOffset(offset);
        return this;
    }

    public String getSort() {
        return sort;
    }

    public void setSort(String sort) {
        this.sort = sort == null ? "" : sort.trim();
    }

    public Flipper sort(String sort) {
        setSort(sort);
        return this;
    }

    public static Flipper sortIfAbsent(Flipper flipper, String sort) {
        if (flipper != null) return flipper.sortIfAbsent(sort);
        return flipper;
    }

    public Flipper sortIfAbsent(String sort) {
        if (this.sort == null || this.sort.isEmpty()) {
            this.sort = sort;
        }
        return this;
    }

}
