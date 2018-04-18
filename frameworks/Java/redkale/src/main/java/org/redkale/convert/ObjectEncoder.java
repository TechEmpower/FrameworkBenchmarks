/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.lang.reflect.*;
import java.util.*;
import org.redkale.util.*;

/**
 * 自定义对象的序列化操作类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <W> Writer输出的子类
 * @param <T> 序列化的数据类型
 */
@SuppressWarnings("unchecked")
public final class ObjectEncoder<W extends Writer, T> implements Encodeable<W, T> {

    static final Type[] TYPEZERO = new Type[0];

    protected final Type type;

    protected final Class typeClass;

    protected EnMember[] members;

    protected ConvertFactory factory;

    private boolean inited = false;

    private final Object lock = new Object();

    protected ObjectEncoder(Type type) {
        this.type = type;
        if (type instanceof ParameterizedType) {
            final ParameterizedType pt = (ParameterizedType) type;
            this.typeClass = (Class) pt.getRawType();
        } else if (type instanceof TypeVariable) {
            TypeVariable tv = (TypeVariable) type;
            Type[] ts = tv.getBounds();
            if (ts.length == 1 && ts[0] instanceof Class) {
                this.typeClass = (Class) ts[0];
            } else {
                throw new ConvertException("[" + type + "] is no a class or ParameterizedType");
            }
        } else {
            this.typeClass = (Class) type;
        }
        this.members = new EnMember[0];
    }

    public void init(final ConvertFactory factory) {
        this.factory = factory;
        try {
            if (type == Object.class) return;
            //if (!(type instanceof Class)) throw new ConvertException("[" + type + "] is no a class");
            final Class clazz = this.typeClass;
            final Set<EnMember> list = new HashSet();
            final boolean reversible = factory.isReversible();
            Creator creator = null;
            try {
                creator = factory.loadCreator(this.typeClass);
            } catch (RuntimeException e) {
                if (reversible) throw e;
            }
            final String[] cps = creator == null ? null : ObjectEncoder.findConstructorProperties(creator);
            try {
                ConvertColumnEntry ref;
                for (final Field field : clazz.getFields()) {
                    if (Modifier.isStatic(field.getModifiers())) continue;
                    if (factory.isConvertDisabled(field)) continue;
                    ref = factory.findRef(field);
                    if (ref != null && ref.ignore()) continue;
                    Type t = TypeToken.createClassType(TypeToken.getGenericType(field.getGenericType(), this.type), this.type);
                    EnMember member = new EnMember(createAttribute(factory, clazz, field, null, null), factory.loadEncoder(t));
                    if (ref != null) member.index = ref.getIndex();
                    list.add(member);
                }
                for (final Method method : clazz.getMethods()) {
                    if (Modifier.isStatic(method.getModifiers())) continue;
                    if (Modifier.isAbstract(method.getModifiers())) continue;
                    if (method.isSynthetic()) continue;
                    if (method.getName().length() < 3) continue;
                    if (method.getName().equals("getClass")) continue;
                    if (!method.getName().startsWith("is") && !method.getName().startsWith("get")) continue;
                    if (factory.isConvertDisabled(method)) continue;
                    if (method.getParameterTypes().length != 0) continue;
                    if (method.getReturnType() == void.class) continue;
                    if (reversible && (cps == null || !contains(cps, ConvertFactory.readGetSetFieldName(method)))) {
                        boolean is = method.getName().startsWith("is");
                        try {
                            clazz.getMethod(method.getName().replaceFirst(is ? "is" : "get", "set"), method.getReturnType());
                        } catch (Exception e) {
                            continue;
                        }
                    }
                    ref = factory.findRef(method);
                    if (ref != null && ref.ignore()) continue;
                    Type t = TypeToken.createClassType(TypeToken.getGenericType(method.getGenericReturnType(), this.type), this.type);
                    EnMember member = new EnMember(createAttribute(factory, clazz, null, method, null), factory.loadEncoder(t));
                    if (ref != null) member.index = ref.getIndex();
                    list.add(member);
                }
                this.members = list.toArray(new EnMember[list.size()]);
                Arrays.sort(this.members);

            } catch (Exception ex) {
                throw new ConvertException(ex);
            }
        } finally {
            inited = true;
            synchronized (lock) {
                lock.notifyAll();
            }
        }
    }

    @Override
    public final void convertTo(W out, T value) {
        if (value == null) {
            out.writeObjectNull(null);
            return;
        }
        if (!this.inited) {
            synchronized (lock) {
                try {
                    lock.wait();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        if (value.getClass() != this.typeClass) {
            final Class clz = value.getClass();
            if (out.needWriteClassName()) out.writeClassName(factory.getEntityAlias(clz));
            factory.loadEncoder(clz).convertTo(out, value);
            return;
        }
        out.writeObjectB(value);
        for (EnMember member : members) {
            out.writeObjectField(member, value);
        }
        out.writeObjectE(value);
    }

    @Override
    public final Type getType() {
        return this.type;
    }

    @Override
    public String toString() {
        return "ObjectEncoder{" + "type=" + type + ", members=" + Arrays.toString(members) + '}';
    }

//
//    static Type makeGenericType(final Type type, final Type[] virGenericTypes, final Type[] realGenericTypes) {
//        if (type instanceof Class) {  //e.g. String
//            return type;
//        } else if (type instanceof ParameterizedType) {  //e.g. Map<String, String>
//            final ParameterizedType pt = (ParameterizedType) type;
//            Type[] paramTypes = pt.getActualTypeArguments();
//            final Type[] newTypes = new Type[paramTypes.length];
//            int count = 0;
//            for (int i = 0; i < newTypes.length; i++) {
//                newTypes[i] = makeGenericType(paramTypes[i], virGenericTypes, realGenericTypes);
//                if (paramTypes[i] == newTypes[i]) count++;
//            }
//            if (count == paramTypes.length) return pt;
//            return new ParameterizedType() {
//
//                @Override
//                public Type[] getActualTypeArguments() {
//                    return newTypes;
//                }
//
//                @Override
//                public Type getRawType() {
//                    return pt.getRawType();
//                }
//
//                @Override
//                public Type getOwnerType() {
//                    return pt.getOwnerType();
//                }
//
//            };
//        }
//        if (realGenericTypes == null) return type;
//        if (type instanceof WildcardType) {   // e.g. <? extends Serializable>
//            final WildcardType wt = (WildcardType) type;
//            for (Type f : wt.getUpperBounds()) {
//                for (int i = 0; i < virGenericTypes.length; i++) {
//                    if (virGenericTypes[i] == f) return realGenericTypes.length == 0 ? Object.class : realGenericTypes[i];
//                }
//            }
//        } else if (type instanceof TypeVariable) { // e.g.  <? extends E>
//            for (int i = 0; i < virGenericTypes.length; i++) {
//                if (virGenericTypes[i] == type) return i >= realGenericTypes.length ? Object.class : realGenericTypes[i];
//            }
//        }
//        return type;
//    }
    static boolean contains(String[] values, String value) {
        for (String str : values) {
            if (str.equals(value)) return true;
        }
        return false;
    }

    static String[] findConstructorProperties(Creator creator) {
        if (creator == null) return null;
        try {
            ConstructorParameters cps = creator.getClass().getMethod("create", Object[].class).getAnnotation(ConstructorParameters.class);
            return cps == null ? null : cps.value();
        } catch (Exception e) {
            return null;
        }
    }

    static Attribute createAttribute(final ConvertFactory factory, Class clazz, final Field field, final Method getter, final Method setter) {
        String fieldalias;
        if (field != null) { // public field
            ConvertColumnEntry ref = factory.findRef(field);
            fieldalias = ref == null || ref.name().isEmpty() ? field.getName() : ref.name();
        } else if (getter != null) {
            ConvertColumnEntry ref = factory.findRef(getter);
            String mfieldname = ConvertFactory.readGetSetFieldName(getter);
            if (ref == null) {
                try {
                    ref = factory.findRef(clazz.getDeclaredField(mfieldname));
                } catch (Exception e) {
                }
            }
            fieldalias = ref == null || ref.name().isEmpty() ? mfieldname : ref.name();
        } else { // setter != null
            ConvertColumnEntry ref = factory.findRef(setter);
            String mfieldname = ConvertFactory.readGetSetFieldName(setter);
            if (ref == null) {
                try {
                    ref = factory.findRef(clazz.getDeclaredField(mfieldname));
                } catch (Exception e) {
                }
            }
            fieldalias = ref == null || ref.name().isEmpty() ? mfieldname : ref.name();
        }
        return Attribute.create(clazz, fieldalias, field, getter, setter);
    }

}
