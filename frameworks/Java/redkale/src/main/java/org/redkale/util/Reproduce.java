package org.redkale.util;

import java.lang.reflect.Modifier;
import java.util.Map;
import java.util.function.*;
import static org.redkale.asm.Opcodes.*;
import org.redkale.asm.*;
import static org.redkale.asm.ClassWriter.COMPUTE_FRAMES;

/**
 * JavaBean类对象的拷贝，相同的字段名会被拷贝 <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <D> 目标对象的数据类型
 * @param <S> 源对象的数据类型
 */
public interface Reproduce<D, S> extends BiFunction<D, S, D> {

    @Override
    public D apply(D dest, S src);

    public static <D, S> Reproduce<D, S> create(final Class<D> destClass, final Class<S> srcClass) {
        return create(destClass, srcClass, (BiPredicate) null, (Map<String, String>) null);
    }

    public static <D, S> Reproduce<D, S> create(final Class<D> destClass, final Class<S> srcClass, final Map<String, String> names) {
        return create(destClass, srcClass, (BiPredicate) null, names);
    }

    @SuppressWarnings("unchecked")
    public static <D, S> Reproduce<D, S> create(final Class<D> destClass, final Class<S> srcClass, final Predicate<String> srcColumnPredicate) {
        return create(destClass, srcClass, (sc, m) -> srcColumnPredicate.test(m), (Map<String, String>) null);
    }

    @SuppressWarnings("unchecked")
    public static <D, S> Reproduce<D, S> create(final Class<D> destClass, final Class<S> srcClass, final Predicate<String> srcColumnPredicate, final Map<String, String> names) {
        return create(destClass, srcClass, (sc, m) -> srcColumnPredicate.test(m), names);
    }

    @SuppressWarnings("unchecked")
    public static <D, S> Reproduce<D, S> create(final Class<D> destClass, final Class<S> srcClass, final BiPredicate<Class<S>, String> srcColumnPredicate) {
        return create(destClass, srcClass, srcColumnPredicate, (Map<String, String>) null);
    }

    @SuppressWarnings("unchecked")
    public static <D, S> Reproduce<D, S> create(final Class<D> destClass, final Class<S> srcClass, final BiPredicate<Class<S>, String> srcColumnPredicate, final Map<String, String> names) {
        // ------------------------------------------------------------------------------
        final String supDynName = Reproduce.class.getName().replace('.', '/');
        final String destClassName = destClass.getName().replace('.', '/');
        final String srcClassName = srcClass.getName().replace('.', '/');
        final String destDesc = Type.getDescriptor(destClass);
        final String srcDesc = Type.getDescriptor(srcClass);
        String newDynName = supDynName + "Dyn_" + destClass.getSimpleName() + "_" + srcClass.getSimpleName();
        ClassLoader loader = Thread.currentThread().getContextClassLoader();
        if (String.class.getClassLoader() != destClass.getClassLoader()) {
            loader = destClass.getClassLoader();
            newDynName = destClassName + "_Dyn" + Reproduce.class.getSimpleName() + "_" + srcClass.getSimpleName();
        }
        try {
            return (Reproduce) loader.loadClass(newDynName.replace('/', '.')).getDeclaredConstructor().newInstance();
        } catch (Throwable ex) {
        }
        // ------------------------------------------------------------------------------
        ClassWriter cw = new ClassWriter(COMPUTE_FRAMES);
        FieldVisitor fv;
        MethodVisitor mv;
        AnnotationVisitor av0;

        cw.visit(V1_8, ACC_PUBLIC + ACC_FINAL + ACC_SUPER, newDynName, "Ljava/lang/Object;L" + supDynName + "<" + destDesc + srcDesc + ">;", "java/lang/Object", new String[]{supDynName});

        { // 构造函数
            mv = (cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null));
            //mv.setDebug(true);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
            mv.visitInsn(RETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }
        {
            mv = (cw.visitMethod(ACC_PUBLIC, "apply", "(" + destDesc + srcDesc + ")" + destDesc, null, null));
            //mv.setDebug(true);

            for (java.lang.reflect.Field field : srcClass.getFields()) {
                if (Modifier.isStatic(field.getModifiers())) continue;
                if (Modifier.isFinal(field.getModifiers())) continue;
                if (!Modifier.isPublic(field.getModifiers())) continue;
                final String sfname = field.getName();
                if (srcColumnPredicate != null && !srcColumnPredicate.test(srcClass, sfname)) continue;

                final String dfname = names == null ? sfname : names.getOrDefault(sfname, sfname);
                java.lang.reflect.Method setter = null;
                try {
                    if (!field.getType().equals(destClass.getField(dfname).getType())) continue;
                } catch (Exception e) {
                    try {
                        char[] cs = dfname.toCharArray();
                        cs[0] = Character.toUpperCase(cs[0]);
                        String dfname2 = new String(cs);
                        setter = destClass.getMethod("set" + dfname2, field.getType());
                    } catch (Exception e2) {
                        continue;
                    }
                }
                mv.visitVarInsn(ALOAD, 1);
                mv.visitVarInsn(ALOAD, 2);
                String td = Type.getDescriptor(field.getType());
                mv.visitFieldInsn(GETFIELD, srcClassName, sfname, td);
                if (setter == null) {
                    mv.visitFieldInsn(PUTFIELD, destClassName, dfname, td);
                } else {
                    mv.visitMethodInsn(INVOKEVIRTUAL, destClassName, setter.getName(), Type.getMethodDescriptor(setter), false);
                }
            }

            for (java.lang.reflect.Method getter : srcClass.getMethods()) {
                if (Modifier.isStatic(getter.getModifiers())) continue;
                if (getter.getParameterTypes().length > 0) continue;
                if ("getClass".equals(getter.getName())) continue;
                if (!getter.getName().startsWith("get") && !getter.getName().startsWith("is")) continue;
                final boolean is = getter.getName().startsWith("is");
                String sfname = getter.getName().substring(is ? 2 : 3);
                if (sfname.length() < 2 || Character.isLowerCase(sfname.charAt(1))) {
                    char[] cs = sfname.toCharArray();
                    cs[0] = Character.toLowerCase(cs[0]);
                    sfname = new String(cs);
                }
                if (srcColumnPredicate != null && !srcColumnPredicate.test(srcClass, sfname)) continue;

                final String dfname = names == null ? sfname : names.getOrDefault(sfname, sfname);
                java.lang.reflect.Method setter = null;
                java.lang.reflect.Field srcField = null;
                char[] cs = dfname.toCharArray();
                cs[0] = Character.toUpperCase(cs[0]);
                String dfname2 = new String(cs);
                try {
                    setter = destClass.getMethod("set" + dfname2, getter.getReturnType());
                } catch (Exception e) {
                    try {
                        srcField = destClass.getField(dfname);
                        if (!getter.getReturnType().equals(srcField.getType())) continue;
                    } catch (Exception e2) {
                        continue;
                    }
                }
                mv.visitVarInsn(ALOAD, 1);
                mv.visitVarInsn(ALOAD, 2);
                mv.visitMethodInsn(INVOKEVIRTUAL, srcClassName, getter.getName(), Type.getMethodDescriptor(getter), false);
                if (srcField == null) {
                    mv.visitMethodInsn(INVOKEVIRTUAL, destClassName, setter.getName(), Type.getMethodDescriptor(setter), false);
                } else {
                    mv.visitFieldInsn(PUTFIELD, destClassName, dfname, Type.getDescriptor(getter.getReturnType()));
                }
            }
            mv.visitVarInsn(ALOAD, 1);
            mv.visitInsn(ARETURN);
            mv.visitMaxs(3, 3);
            mv.visitEnd();
        }
        {
            mv = (cw.visitMethod(ACC_PUBLIC + ACC_BRIDGE + ACC_SYNTHETIC, "apply", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;", null, null));
            //mv.setDebug(true);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitTypeInsn(CHECKCAST, destClassName);
            mv.visitVarInsn(ALOAD, 2);
            mv.visitTypeInsn(CHECKCAST, srcClassName);
            mv.visitMethodInsn(INVOKEVIRTUAL, newDynName, "apply", "(" + destDesc + srcDesc + ")" + destDesc, false);
            mv.visitInsn(ARETURN);
            mv.visitMaxs(3, 3);
            mv.visitEnd();
        }
        cw.visitEnd();
        // ------------------------------------------------------------------------------
        byte[] bytes = cw.toByteArray();
        Class<?> creatorClazz = new ClassLoader(loader) {
            public final Class<?> loadClass(String name, byte[] b) {
                return defineClass(name, b, 0, b.length);
            }
        }.loadClass(newDynName.replace('/', '.'), bytes);
        try {
            return (Reproduce) creatorClazz.getDeclaredConstructor().newInstance();
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

}
