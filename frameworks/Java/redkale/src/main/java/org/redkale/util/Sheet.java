/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.util;

import java.util.*;
import java.util.function.*;
import java.util.stream.*;

/**
 * 页集合。 结构由一个total总数和一个List列表组合而成。
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> 集合元素的数据类型
 */
@SuppressWarnings("unchecked")
public class Sheet<T> implements java.io.Serializable, Iterable<T> {

    private long total = -1;

    private Collection<T> rows;

    public Sheet() {
        super();
    }

    public Sheet(int total, Collection<? extends T> data) {
        this((long) total, data);
    }

    public Sheet(long total, Collection<? extends T> data) {
        this.total = total;
        this.rows = (Collection<T>) data;
    }

    public static <E> Sheet<E> asSheet(Collection<E> data) {
        return data == null ? new Sheet() : new Sheet(data.size(), data);
    }

    public static <E> Sheet<E> empty() {
        return new Sheet<>();
    }

    public Sheet<T> copyTo(Sheet<T> copy) {
        if (copy == null) return copy;
        copy.total = this.total;
        if (this.getRows() != null) {
            copy.setRows(new ArrayList(this.getRows()));
        } else {
            copy.rows = null;
        }
        return copy;
    }

    /**
     * 判断数据列表是否为空
     *
     * @return 是否为空
     */
    public boolean isEmpty() {
        return this.rows == null || this.rows.isEmpty();
    }

    @Override
    public String toString() {
        return "{\"total\":" + this.total + ", \"rows\":" + this.rows + "}";
    }

    public long getTotal() {
        return this.total;
    }

    public void setTotal(long total) {
        this.total = total;
    }

    public Collection<T> getRows() {
        return this.rows;
    }

    public List<T> list() {
        return list(false);
    }

    public List<T> list(boolean created) {
        if (this.rows == null) return created ? new ArrayList() : null;
        return (this.rows instanceof List) ? (List<T>) this.rows : new ArrayList(this.rows);
    }

    public void setRows(Collection<? extends T> data) {
        this.rows = (Collection<T>) data;
    }

    @Override
    public Iterator<T> iterator() {
        return (this.rows == null) ? new ArrayList<T>().iterator() : this.rows.iterator();
    }

    @Override
    public void forEach(final Consumer<? super T> consumer) {
        if (consumer != null && this.rows != null && !this.rows.isEmpty()) {
            this.rows.forEach(consumer);
        }
    }

    public <R> Sheet<R> map(Function<T, R> mapper) {
        if (this.isEmpty()) return (Sheet) this;
        final List<R> list = new ArrayList<>();
        for (T item : this.rows) {
            list.add(mapper.apply(item));
        }
        return new Sheet<>(getTotal(), list);
    }

    public void forEachParallel(final Consumer<? super T> consumer) {
        if (consumer != null && this.rows != null && !this.rows.isEmpty()) {
            this.rows.parallelStream().forEach(consumer);
        }
    }

    @Override
    public Spliterator<T> spliterator() {
        return (this.rows == null) ? new ArrayList<T>().spliterator() : this.rows.spliterator();
    }

    public Stream<T> stream() {
        return (this.rows == null) ? new ArrayList<T>().stream() : this.rows.stream();
    }

    public Stream<T> parallelStream() {
        return (this.rows == null) ? new ArrayList<T>().parallelStream() : this.rows.parallelStream();
    }

    public Object[] toArray() {
        return (this.rows == null) ? new ArrayList<T>().toArray() : this.rows.toArray();
    }

    public T[] toArray(T[] a) {
        return (this.rows == null) ? new ArrayList<T>().toArray(a) : this.rows.toArray(a);
    }
}
