/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.reflect.*;
import org.redkale.util.Attribute;

/**
 * 字段的序列化操作类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <W> Writer输出的子类
 * @param <T> 字段依附的类
 * @param <F> 字段的数据类型
 */
@SuppressWarnings("unchecked")
public final class EnMember<W extends Writer, T, F> implements Comparable<EnMember<W, T, F>> {

    final Attribute<T, F> attribute;

    final Encodeable<W, F> encoder;

    final boolean istring;

    //final boolean isnumber;
    final boolean isbool;

    protected int index;

    public EnMember(Attribute<T, F> attribute, Encodeable<W, F> encoder) {
        this.attribute = attribute;
        this.encoder = encoder;
        Class t = attribute.type();
        this.istring = CharSequence.class.isAssignableFrom(t);
        this.isbool = t == Boolean.class || t == boolean.class;
        //this.isnumber = Number.class.isAssignableFrom(t) || (!this.isbool && t.isPrimitive());
    }

    public static <W extends Writer, T, F> EnMember<W, T, F> create(final ConvertFactory factory, final Class<T> clazz, final String fieldname) {
        try {
            Field field = clazz.getDeclaredField(fieldname);
            return new EnMember<>(Attribute.create(field), factory.loadEncoder(field.getGenericType()));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static <W extends Writer, T, F> EnMember<W, T, F> create(final ConvertFactory factory, final Class<T> clazz, final String fieldname, final Class<F> fieldtype) {
        return new EnMember<>(Attribute.create(clazz, fieldname, fieldtype), factory.loadEncoder(fieldtype));
    }

    public static <W extends Writer, T, F> EnMember<W, T, F> create(final Attribute<T, F> attribute, final ConvertFactory factory, final Class<F> fieldtype) {
        return new EnMember<>(attribute, factory.loadEncoder(fieldtype));
    }

    public final boolean match(String name) {
        return attribute.field().equals(name);
    }

    public int getIndex() {
        return this.index;
    }

    @Override
    public final int compareTo(EnMember<W, T, F> o) {
        if (o == null) return -1;
        if (this.index != o.index) return (this.index == 0 ? Integer.MAX_VALUE : this.index) - (o.index == 0 ? Integer.MAX_VALUE : o.index);
        return this.attribute.field().compareTo(o.attribute.field());
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!(obj instanceof EnMember)) return false;
        EnMember other = (EnMember) obj;
        return compareTo(other) == 0;
    }

    @Override
    public int hashCode() {
        return this.attribute.field().hashCode();
    }

    @Override
    public String toString() {
        return "EnMember{" + "attribute=" + attribute.field() + ", encoder=" + (encoder == null ? null : encoder.getClass().getName()) + '}';
    }
}
