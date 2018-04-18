/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.util;

import java.lang.reflect.Type;
import java.lang.reflect.*;
import java.util.*;
import org.redkale.asm.*;
import static org.redkale.asm.ClassWriter.COMPUTE_FRAMES;
import static org.redkale.asm.Opcodes.*;

/**
 *
 * 获取泛型的Type类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> 泛型
 */
public abstract class TypeToken<T> {

    private final Type type;

    public TypeToken() {
        type = ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
    }

    public final Type getType() {
        return type;
    }

    /**
     * 判断Type是否能确定最终的class， 是则返回true，存在通配符或者不确定类型则返回false。
     * 例如: Map&#60; String, String &#62; 返回 ture; Map&#60; ? extends Serializable, String &#62; 返回false;
     *
     * @param type Type对象
     *
     * @return 是否可反解析
     */
    public final static boolean isClassType(final Type type) {
        if (type instanceof Class) return true;
        if (type instanceof WildcardType) return false;
        if (type instanceof TypeVariable) return false;
        if (type instanceof GenericArrayType) return isClassType(((GenericArrayType) type).getGenericComponentType());
        if (!(type instanceof ParameterizedType)) return false; //只能是null了
        final ParameterizedType ptype = (ParameterizedType) type;
        if (ptype.getOwnerType() != null && !isClassType(ptype.getOwnerType())) return false;
        if (!isClassType(ptype.getRawType())) return false;
        for (Type t : ptype.getActualTypeArguments()) {
            if (!isClassType(t)) return false;
        }
        return true;
    }

    public static Type[] getGenericType(final Type[] types, final Type declaringClass) {
        Type[] newTypes = new Type[types.length];
        for (int i = 0; i < newTypes.length; i++) {
            newTypes[i] = getGenericType(types[i], declaringClass);
        }
        return newTypes;
    }

    /**
     * 获取TypeVariable对应的实际Type, 如果type不是TypeVariable 直接返回type。
     * <pre>
     *  public abstract class Key {
     *  }
     *  public abstract class Val {
     *  }
     *  public abstract class AService &lt;K extends Key, V extends Val&gt; {
     *       public abstract V findValue(K key);
     *       public abstract Sheet&lt;V&gt; queryValue(K key);
     *  }
     *  public class Key2 extends Key {
     *  }
     *  public class Val2 extends Val {
     *  }
     *  public class Service2 extends Service &lt;Key2, Val2&gt; {
     *       public Val2 findValue(Key2 key){
     *          return new Val2();
     *       }
     *       public Sheet&lt;Val2&gt; queryValue(Key2 key){
     *          return new Sheet();
     *       }
     *  }
     * </pre>
     *
     *
     * @param type           泛型
     * @param declaringClass 泛型依附类
     *
     * @return Type
     */
    public static Type getGenericType(final Type type, final Type declaringClass) {
        if (type == null || declaringClass == null) return type;
        if (type instanceof TypeVariable) {
            Type superType = null;
            Class declaringClass0 = null;
            if (declaringClass instanceof Class) {
                declaringClass0 = (Class) declaringClass;
                superType = declaringClass0.getGenericSuperclass();
                while (superType instanceof Class && superType != Object.class) superType = ((Class) superType).getGenericSuperclass();
            } else if (declaringClass instanceof ParameterizedType) {
                superType = declaringClass;
                Type rawType = ((ParameterizedType) declaringClass).getRawType();
                if (rawType instanceof Class) declaringClass0 = (Class) rawType;
            }
            if (declaringClass0 != null && superType instanceof ParameterizedType) {
                ParameterizedType superPT = (ParameterizedType) superType;
                Type[] atas = superPT.getActualTypeArguments();
                Class ss = declaringClass0;
                TypeVariable[] asts = ss.getTypeParameters();
                while (atas.length != asts.length && ss != Object.class) {
                    ss = ss.getSuperclass();
                    asts = ss.getTypeParameters();
                }
                if (atas.length == asts.length) {
                    for (int i = 0; i < asts.length; i++) {
                        if (asts[i] == type) return atas[i];
                    }
                }
            }
            TypeVariable tv = (TypeVariable) type;
            if (tv.getBounds().length == 1) return tv.getBounds()[0];
        }
        if (type instanceof ParameterizedType) {
            ParameterizedType pt = (ParameterizedType) type;
            return createParameterizedType(getGenericType(pt.getOwnerType(), declaringClass),
                getGenericType(pt.getRawType(), declaringClass),
                getGenericType(pt.getActualTypeArguments(), declaringClass));
        }
        return type;
    }

    /**
     * 动态创建类型为ParameterizedType或Class的Type
     *
     * @param type           当前泛型
     * @param declaringType0 子类
     *
     * @return Type
     */
    public static Type createClassType(final Type type, final Type declaringType0) {
        if (isClassType(type)) return type;
        if (type instanceof ParameterizedType) {  // e.g. Map<String, String>
            final ParameterizedType pt = (ParameterizedType) type;
            final Type[] paramTypes = pt.getActualTypeArguments();
            for (int i = 0; i < paramTypes.length; i++) {
                paramTypes[i] = createClassType(paramTypes[i], declaringType0);
            }
            return createParameterizedType(pt.getOwnerType(), pt.getRawType(), paramTypes);
        }
        Type declaringType = declaringType0;
        if (declaringType instanceof Class) {
            do {
                declaringType = ((Class) declaringType).getGenericSuperclass();
                if (declaringType == Object.class) return Object.class;
            } while (declaringType instanceof Class);
        }
        //存在通配符则declaringType 必须是 ParameterizedType
        if (!(declaringType instanceof ParameterizedType)) return Object.class;
        final ParameterizedType declaringPType = (ParameterizedType) declaringType;
        final Type[] virTypes = ((Class) declaringPType.getRawType()).getTypeParameters();
        final Type[] desTypes = declaringPType.getActualTypeArguments();
        if (type instanceof WildcardType) {   // e.g. <? extends Serializable>
            final WildcardType wt = (WildcardType) type;
            for (Type f : wt.getUpperBounds()) {
                for (int i = 0; i < virTypes.length; i++) {
                    if (virTypes[i].equals(f)) return desTypes.length <= i ? Object.class : desTypes[i];
                }
            }
        } else if (type instanceof TypeVariable) { // e.g.  <? extends E>
            for (int i = 0; i < virTypes.length; i++) {
                if (virTypes[i].equals(type)) return desTypes.length <= i ? Object.class : desTypes[i];
            }
        }
        return type;
    }

    /**
     * 动态创建 ParameterizedType
     *
     * @param ownerType0           ParameterizedType 的 ownerType
     * @param rawType0             ParameterizedType 的 rawType
     * @param actualTypeArguments0 ParameterizedType 的 actualTypeArguments
     *
     * @return Type
     */
    public static Type createParameterizedType(final Type ownerType0, final Type rawType0, final Type... actualTypeArguments0) {
        if (ownerType0 == null && rawType0 instanceof Class) {
            int count = 0;
            for (Type t : actualTypeArguments0) {
                if (isClassType(t)) count++;
            }
            if (count == actualTypeArguments0.length) return createParameterizedType((Class) rawType0, actualTypeArguments0);
        }
        return new ParameterizedType() {
            private final Class<?> rawType = (Class<?>) rawType0;

            private final Type ownerType = ownerType0;

            private final Type[] actualTypeArguments = actualTypeArguments0;

            @Override
            public Type[] getActualTypeArguments() {
                return actualTypeArguments.clone();
            }

            @Override
            public Type getRawType() {
                return rawType;
            }

            @Override
            public Type getOwnerType() {
                return ownerType;
            }

            @Override
            public int hashCode() {
                return Arrays.hashCode(actualTypeArguments) ^ Objects.hashCode(rawType) ^ Objects.hashCode(ownerType);
            }

            @Override
            public boolean equals(Object o) {
                if (!(o instanceof ParameterizedType)) return false;
                final ParameterizedType that = (ParameterizedType) o;
                if (this == that) return true;
                return Objects.equals(ownerType, that.getOwnerType())
                    && Objects.equals(rawType, that.getRawType())
                    && Arrays.equals(actualTypeArguments, that.getActualTypeArguments());
            }

            @Override
            public String toString() {
                StringBuilder sb = new StringBuilder();
                if (ownerType != null) sb.append((ownerType instanceof Class) ? (((Class) ownerType).getName()) : ownerType.toString()).append(".");
                sb.append(rawType.getName());

                if (actualTypeArguments != null && actualTypeArguments.length > 0) {
                    sb.append("<");
                    boolean first = true;
                    for (Type t : actualTypeArguments) {
                        if (!first) sb.append(", ");
                        sb.append(t);
                        first = false;
                    }
                    sb.append(">");
                }
                return sb.toString();
            }
        };
    }

    private static Type createParameterizedType(final Class rawType, final Type... actualTypeArguments) {
        ClassLoader loader = Thread.currentThread().getContextClassLoader();
        String newDynName = TypeToken.class.getName().replace('.', '/') + "_Dyn" + System.currentTimeMillis();
        for (;;) {
            try {
                loader.loadClass(newDynName.replace('/', '.'));
                newDynName = TypeToken.class.getName().replace('.', '/') + "_Dyn" + Math.abs(System.nanoTime());
            } catch (Throwable ex) {  //异常说明类不存在
                break;
            }
        }
        ClassWriter cw = new ClassWriter(COMPUTE_FRAMES);
        FieldVisitor fv;
        MethodVisitor mv;
        cw.visit(V1_8, ACC_PUBLIC + ACC_FINAL + ACC_SUPER, newDynName, null, "java/lang/Object", null);
        String rawTypeDesc = org.redkale.asm.Type.getDescriptor(rawType);
        StringBuilder sb = new StringBuilder();
        sb.append(rawTypeDesc.substring(0, rawTypeDesc.length() - 1)).append('<');
        for (Type c : actualTypeArguments) {
            sb.append(getClassTypeDescriptor(c));
        }
        sb.append(">;");
        {
            fv = cw.visitField(ACC_PUBLIC, "field", rawTypeDesc, sb.toString(), null);
            fv.visitEnd();
        }
        {//构造方法
            mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
            mv.visitInsn(RETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }
        cw.visitEnd();
        byte[] bytes = cw.toByteArray();
        Class<?> newClazz = new ClassLoader(loader) {
            public final Class<?> loadClass(String name, byte[] b) {
                return defineClass(name, b, 0, b.length);
            }
        }.loadClass(newDynName.replace('/', '.'), bytes);
        try {
            return newClazz.getField("field").getGenericType();
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    private static CharSequence getClassTypeDescriptor(Type type) {
        if (!isClassType(type)) throw new IllegalArgumentException(type + " not a class type");
        if (type instanceof Class) return org.redkale.asm.Type.getDescriptor((Class) type);
        final ParameterizedType pt = (ParameterizedType) type;
        CharSequence rawTypeDesc = getClassTypeDescriptor(pt.getRawType());
        StringBuilder sb = new StringBuilder();
        sb.append(rawTypeDesc.subSequence(0, rawTypeDesc.length() - 1)).append('<');
        for (Type c : pt.getActualTypeArguments()) {
            sb.append(getClassTypeDescriptor(c));
        }
        sb.append(">;");
        return sb;
    }
}
