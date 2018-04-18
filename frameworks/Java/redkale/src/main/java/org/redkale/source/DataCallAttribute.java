/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.io.*;
import java.lang.reflect.*;
import java.util.concurrent.*;
import org.redkale.util.*;

/**
 *
 * <p> 详情见: https://redkale.org
 * @author zhangjx
 */
public class DataCallAttribute implements Attribute<Object, Serializable> {

    public static final DataCallAttribute instance = new DataCallAttribute();

    private static final ConcurrentHashMap<Class, Attribute> attributes = new ConcurrentHashMap<>();

    static <T> Attribute<T, Serializable> load(final Class clazz) {
        Attribute rs = attributes.get(clazz);
        if (rs != null) return rs;
        synchronized (attributes) {
            rs = attributes.get(clazz);
            if (rs == null) {
                Class cltmp = clazz;
                do {
                    for (Field field : cltmp.getDeclaredFields()) {
                        if (field.getAnnotation(javax.persistence.GeneratedValue.class) == null) continue;
                        try {
                            rs = Attribute.create(cltmp, field);
                            attributes.put(clazz, rs);
                            return rs;
                        } catch (RuntimeException e) {
                        }
                    }
                } while ((cltmp = cltmp.getSuperclass()) != Object.class);
            }
            return rs;
        }
    }

    @Override
    public Class<Serializable> type() {
        return Serializable.class;
    }

    @Override
    public Class<Object> declaringClass() {
        return Object.class;
    }

    @Override
    public String field() {
        return "";
    }

    @Override
    public Serializable get(final Object obj) {
        if (obj == null) return null;
        return load(obj.getClass()).get(obj);
    }

    @Override
    public void set(final Object obj, final Serializable key) {
        if (obj == null) return;
        load(obj.getClass()).set(obj, key);
    }
}
