/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.util;

import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.util.*;
import java.util.AbstractMap.SimpleEntry;
import java.util.concurrent.*;
import java.util.logging.*;
import org.redkale.asm.*;
import org.redkale.asm.Type;
import static org.redkale.asm.Opcodes.*;

/**
 * <p>
 * 实现一个类的构造方法。 代替低效的反射实现方式。 不支持数组类。
 * 常见的无参数的构造函数类都可以自动生成Creator， 对应自定义的类可以提供一个静态构建Creator方法。
 * 例如:
 * <blockquote><pre>
 * public class Record {
 *
 *    private final int id;
 *
 *    private String name;
 *
 *    Record(int id, String name) {
 *        this.id = id;
 *        this.name = name;
 *    }
 *
 *    private static Creator createCreator() {
 *        return new Creator&lt;Record&gt;() {
 *            &#64;Override
 *            &#64;ConstructorParameters({"id", "name"})
 *            public Record create(Object... params) {
 *                if(params[0] == null) params[0] = 0;
 *                return new Record((Integer) params[0], (String) params[1]);
 *            }
 *         };
 *    }
 * }
 * </pre></blockquote>
 *
 * 或者:
 * <blockquote><pre>
 * public class Record {
 *
 *    private final int id;
 *
 *    private String name;
 *
 *    &#64;ConstructorParameters({"id", "name"})
 *    public Record(int id, String name) {
 *        this.id = id;
 *        this.name = name;
 *    }
 * }
 * </pre></blockquote>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> 构建对象的数据类型
 */
public interface Creator<T> {

    @SuppressWarnings("unchecked")
    static class CreatorInner {

        static final Logger logger = Logger.getLogger(Creator.class.getSimpleName());

        static final Map<Class, Creator> creatorCacheMap = new HashMap<>();

        static {
            creatorCacheMap.put(ArrayList.class, (params) -> new ArrayList<>());
            creatorCacheMap.put(HashMap.class, (params) -> new HashMap<>());
            creatorCacheMap.put(HashSet.class, (params) -> new HashSet<>());
            creatorCacheMap.put(ConcurrentHashMap.class, (params) -> new ConcurrentHashMap<>());
            creatorCacheMap.put(CompletableFuture.class, (params) -> new CompletableFuture<>());
        }

        static class SimpleClassVisitor extends ClassVisitor {

            private final String constructorDesc;

            private final List<String> fieldnames;

            private boolean started;

            public SimpleClassVisitor(int api, List<String> fieldnames, String constructorDesc) {
                super(api);
                this.fieldnames = fieldnames;
                this.constructorDesc = constructorDesc;
            }

            @Override
            public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
                if (java.lang.reflect.Modifier.isStatic(access) || !"<init>".equals(name)) return null;
                if (constructorDesc != null && !constructorDesc.equals(desc)) return null;
                if (this.started) return null;
                this.started = true;
                //返回的List中参数列表可能会比方法参数量多，因为方法内的临时变量也会存入list中， 所以需要list的元素集合比方法的参数多
                return new MethodVisitor(Opcodes.ASM6) {
                    @Override
                    public void visitLocalVariable(String name, String description, String signature, Label start, Label end, int index) {
                        if (index < 1) return;
                        int size = fieldnames.size();
                        //index不会按顺序执行的
                        if (index > size) {
                            for (int i = size; i < index; i++) {
                                fieldnames.add(" ");
                            }
                            fieldnames.set(index - 1, name);
                        }
                        fieldnames.set(index - 1, name);
                    }
                };
            }
        }

        public static SimpleEntry<String, Class>[] getConstructorField(Class clazz, int paramcount, String constructorDesc) {
            String n = clazz.getName();
            InputStream in = clazz.getResourceAsStream(n.substring(n.lastIndexOf('.') + 1) + ".class");
            if (in == null) return null;
            ByteArrayOutputStream out = new ByteArrayOutputStream(1024);
            byte[] bytes = new byte[1024];
            int pos;
            try {
                while ((pos = in.read(bytes)) != -1) {
                    out.write(bytes, 0, pos);
                }
                in.close();
            } catch (IOException io) {
                return null;
            }
            final List<String> fieldnames = new ArrayList<>();
            new ClassReader(out.toByteArray()).accept(new SimpleClassVisitor(Opcodes.ASM6, fieldnames, constructorDesc), 0);
            if (fieldnames.isEmpty()) return null;
            if (paramcount == fieldnames.size()) {
                return getConstructorField(clazz, paramcount, fieldnames.toArray(new String[fieldnames.size()]));
            } else {
                String[] fs = new String[paramcount];
                for (int i = 0; i < fs.length; i++) {
                    fs[i] = fieldnames.get(i);
                }
                return getConstructorField(clazz, paramcount, fs);
            }
        }

        public static SimpleEntry<String, Class>[] getConstructorField(Class clazz, int paramcount, String[] names) {
            SimpleEntry<String, Class>[] se = new SimpleEntry[names.length];
            for (int i = 0; i < names.length; i++) { //查询参数名对应的Field
                try {
                    Field field = clazz.getDeclaredField(names[i]);
                    se[i] = new SimpleEntry<>(field.getName(), field.getType());
                } catch (NoSuchFieldException fe) {
                    Class cz = clazz;
                    Field field = null;
                    while ((cz = cz.getSuperclass()) != Object.class) {
                        try {
                            field = cz.getDeclaredField(names[i]);
                            break;
                        } catch (NoSuchFieldException nsfe) {
                        }
                    }
                    if (field == null) return null;
                    se[i] = new SimpleEntry<>(field.getName(), field.getType());
                } catch (Exception e) {
                    if (logger.isLoggable(Level.FINE)) logger.log(Level.FINE, clazz + " getConstructorField error", e);
                    return null;
                }
            }
            return se;
        }

        public static SimpleEntry<String, Class>[] getConstructorField(Class clazz, int paramcount, Parameter[] params) {
            SimpleEntry<String, Class>[] se = new SimpleEntry[params.length];
            for (int i = 0; i < params.length; i++) { //查询参数名对应的Field
                try {
                    Field field = clazz.getDeclaredField(params[i].getName());
                    se[i] = new SimpleEntry<>(field.getName(), field.getType());
                } catch (Exception e) {
                    return null;
                }
            }
            return se;
        }
    }

    /**
     * 创建对象
     *
     * @param params 构造函数的参数
     *
     * @return 构建的对象
     */
    public T create(Object... params);

    /**
     * 根据指定的class采用ASM技术生产Creator。
     *
     * @param <T>   构建类的数据类型
     * @param clazz 构建类
     *
     * @return Creator对象
     */
    @SuppressWarnings("unchecked")
    public static <T> Creator<T> create(Class<T> clazz) {
        if (clazz.isAssignableFrom(ArrayList.class)) {
            clazz = (Class<T>) ArrayList.class;
        } else if (clazz.isAssignableFrom(HashMap.class)) {
            clazz = (Class<T>) HashMap.class;
        } else if (clazz.isAssignableFrom(HashSet.class)) {
            clazz = (Class<T>) HashSet.class;
        } else if (clazz.isAssignableFrom(ConcurrentHashMap.class)) {
            clazz = (Class<T>) ConcurrentHashMap.class;
        }
        Creator creator = CreatorInner.creatorCacheMap.get(clazz);
        if (creator != null) return creator;
        if (clazz.isInterface() || Modifier.isAbstract(clazz.getModifiers())) {
            throw new RuntimeException("[" + clazz + "] is a interface or abstract class, cannot create it's Creator.");
        }
        for (final Method method : clazz.getDeclaredMethods()) { //查找类中是否存在提供创建Creator实例的静态方法
            if (!Modifier.isStatic(method.getModifiers())) continue;
            if (method.getParameterTypes().length != 0) continue;
            if (method.getReturnType() != Creator.class) continue;
            try {
                method.setAccessible(true);
                return (Creator<T>) method.invoke(null);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        final String supDynName = Creator.class.getName().replace('.', '/');
        final String interName = clazz.getName().replace('.', '/');
        final String interDesc = Type.getDescriptor(clazz);
        ClassLoader loader = Thread.currentThread().getContextClassLoader();
        String newDynName = supDynName + "_" + clazz.getSimpleName() + "_" + (System.currentTimeMillis() % 10000);
        if (String.class.getClassLoader() != clazz.getClassLoader()) {
            loader = clazz.getClassLoader();
            newDynName = interName + "_Dyn" + Creator.class.getSimpleName();
        }
        try {
            return (Creator) loader.loadClass(newDynName.replace('/', '.')).getDeclaredConstructor().newInstance();
        } catch (Throwable ex) {
        }

        Constructor<T> constructor0 = null;
        SimpleEntry<String, Class>[] constructorParameters0 = null; //构造函数的参数

        if (constructor0 == null) {  // 1、查找public的空参数构造函数
            for (Constructor c : clazz.getConstructors()) {
                if (c.getParameterCount() == 0) {
                    constructor0 = c;
                    constructorParameters0 = new SimpleEntry[0];
                    break;
                }
            }
        }
        if (constructor0 == null) {  // 2、查找public带ConstructorParameters注解的构造函数
            for (Constructor c : clazz.getConstructors()) {
                ConstructorParameters cp = (ConstructorParameters) c.getAnnotation(ConstructorParameters.class);
                if (cp == null) continue;
                SimpleEntry<String, Class>[] fields = CreatorInner.getConstructorField(clazz, c.getParameterCount(), cp.value());
                if (fields != null) {
                    constructor0 = c;
                    constructorParameters0 = fields;
                    break;
                }
            }
        }
        if (constructor0 == null) {  // 3、查找public且不带ConstructorParameters注解的构造函数
            List<Constructor> cs = new ArrayList<>();
            for (Constructor c : clazz.getConstructors()) {
                if (c.getAnnotation(ConstructorParameters.class) != null) continue;
                if (c.getParameterCount() < 1) continue;
                cs.add(c);
            }
            //优先参数最多的构造函数
            cs.sort((o1, o2) -> o2.getParameterCount() - o1.getParameterCount());
            for (Constructor c : cs) {
                SimpleEntry<String, Class>[] fields = CreatorInner.getConstructorField(clazz, c.getParameterCount(), Type.getConstructorDescriptor(c));
                if (fields != null) {
                    constructor0 = c;
                    constructorParameters0 = fields;
                    break;
                }
            }
        }
        if (constructor0 == null) {  // 4、查找非private带ConstructorParameters的构造函数
            for (Constructor c : clazz.getDeclaredConstructors()) {
                if (Modifier.isPublic(c.getModifiers()) || Modifier.isPrivate(c.getModifiers())) continue;
                ConstructorParameters cp = (ConstructorParameters) c.getAnnotation(ConstructorParameters.class);
                if (cp == null) continue;
                SimpleEntry<String, Class>[] fields = CreatorInner.getConstructorField(clazz, c.getParameterCount(), cp.value());
                if (fields != null) {
                    constructor0 = c;
                    constructorParameters0 = fields;
                    break;
                }
            }
        }
        if (constructor0 == null) {  // 5、查找非private且不带ConstructorParameters的构造函数
            List<Constructor> cs = new ArrayList<>();
            for (Constructor c : clazz.getDeclaredConstructors()) {
                if (Modifier.isPublic(c.getModifiers()) || Modifier.isPrivate(c.getModifiers())) continue;
                if (c.getAnnotation(ConstructorParameters.class) != null) continue;
                if (c.getParameterCount() < 1) continue;
                cs.add(c);
            }
            //优先参数最多的构造函数
            cs.sort((o1, o2) -> o2.getParameterCount() - o1.getParameterCount());
            for (Constructor c : cs) {
                SimpleEntry<String, Class>[] fields = CreatorInner.getConstructorField(clazz, c.getParameterCount(), Type.getConstructorDescriptor(c));
                if (fields != null) {
                    constructor0 = c;
                    constructorParameters0 = fields;
                    break;
                }
            }
        }
        final Constructor<T> constructor = constructor0;
        final SimpleEntry<String, Class>[] constructorParameters = constructorParameters0;
        if (constructor == null || constructorParameters == null) {
            throw new RuntimeException("[" + clazz + "] have no public or ConstructorParameters-Annotation constructor.");
        }
        //-------------------------------------------------------------
        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
        FieldVisitor fv;
        MethodVisitor mv;
        AnnotationVisitor av0;
        cw.visit(V1_8, ACC_PUBLIC + ACC_FINAL + ACC_SUPER, newDynName, "Ljava/lang/Object;L" + supDynName + "<" + interDesc + ">;", "java/lang/Object", new String[]{supDynName});

        {//Creator自身的构造方法
            mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
            mv.visitInsn(RETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }
        {//create 方法
            mv = cw.visitMethod(ACC_PUBLIC + ACC_VARARGS, "create", "([Ljava/lang/Object;)L" + interName + ";", null, null);
            if (constructorParameters.length > 0) {
                av0 = mv.visitAnnotation(Type.getDescriptor(ConstructorParameters.class), true);
                AnnotationVisitor av1 = av0.visitArray("value");
                for (SimpleEntry<String, Class> n : constructorParameters) {
                    av1.visit(null, n.getKey());
                }
                av1.visitEnd();
                av0.visitEnd();
            }
            final int[] iconsts = {ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5};
            {  //有Primitive数据类型且值为null的参数需要赋默认值
                for (int i = 0; i < constructorParameters.length; i++) {
                    final Class pt = constructorParameters[i].getValue();
                    if (!pt.isPrimitive()) continue;
                    mv.visitVarInsn(ALOAD, 1);
                    if (i < 6) {
                        mv.visitInsn(iconsts[i]);
                    } else if (i <= Byte.MAX_VALUE) {
                        mv.visitIntInsn(BIPUSH, i);
                    } else if (i <= Short.MAX_VALUE) {
                        mv.visitIntInsn(SIPUSH, i);
                    } else {
                        mv.visitLdcInsn(i);
                    }
                    mv.visitInsn(AALOAD);
                    Label lab = new Label();
                    mv.visitJumpInsn(IFNONNULL, lab);
                    mv.visitVarInsn(ALOAD, 1);
                    if (i < 6) {
                        mv.visitInsn(iconsts[i]);
                    } else if (i <= Byte.MAX_VALUE) {
                        mv.visitIntInsn(BIPUSH, i);
                    } else if (i <= Short.MAX_VALUE) {
                        mv.visitIntInsn(SIPUSH, i);
                    } else {
                        mv.visitLdcInsn(i);
                    }
                    if (pt == int.class) {
                        mv.visitInsn(ICONST_0);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false);
                    } else if (pt == long.class) {
                        mv.visitInsn(LCONST_0);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;", false);
                    } else if (pt == boolean.class) {
                        mv.visitFieldInsn(GETSTATIC, "java/lang/Boolean", "FALSE", "Ljava/lang/Boolean;");
                    } else if (pt == short.class) {
                        mv.visitInsn(ICONST_0);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Short", "valueOf", "(S)Ljava/lang/Short;", false);
                    } else if (pt == float.class) {
                        mv.visitInsn(FCONST_0);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Float", "valueOf", "(F)Ljava/lang/Float;", false);
                    } else if (pt == byte.class) {
                        mv.visitInsn(ICONST_0);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Byte", "valueOf", "(B)Ljava/lang/Byte;", false);
                    } else if (pt == double.class) {
                        mv.visitInsn(DCONST_0);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Double", "valueOf", "(D)Ljava/lang/Double;", false);
                    } else if (pt == char.class) {
                        mv.visitInsn(ICONST_0);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Character", "valueOf", "(C)Ljava/lang/Character;", false);
                    }
                    mv.visitInsn(AASTORE);
                    mv.visitLabel(lab);
                }
            }
            mv.visitTypeInsn(NEW, interName);
            mv.visitInsn(DUP);
            //---------------------------------------
            {
                for (int i = 0; i < constructorParameters.length; i++) {
                    mv.visitVarInsn(ALOAD, 1);
                    if (i < 6) {
                        mv.visitInsn(iconsts[i]);
                    } else if (i <= Byte.MAX_VALUE) {
                        mv.visitIntInsn(BIPUSH, i);
                    } else if (i <= Short.MAX_VALUE) {
                        mv.visitIntInsn(SIPUSH, i);
                    } else {
                        mv.visitLdcInsn(i);
                    }
                    mv.visitInsn(AALOAD);
                    final Class ct = constructorParameters[i].getValue();
                    if (ct.isPrimitive()) {
                        final Class bigct = Array.get(Array.newInstance(ct, 1), 0).getClass();
                        mv.visitTypeInsn(CHECKCAST, bigct.getName().replace('.', '/'));
                        try {
                            Method pm = bigct.getMethod(ct.getSimpleName() + "Value");
                            mv.visitMethodInsn(INVOKEVIRTUAL, bigct.getName().replace('.', '/'), pm.getName(), Type.getMethodDescriptor(pm), false);
                        } catch (Exception ex) {
                            throw new RuntimeException(ex); //不可能会发生
                        }
                    } else {
                        mv.visitTypeInsn(CHECKCAST, ct.getName().replace('.', '/'));
                    }
                }
            }
            //---------------------------------------
            mv.visitMethodInsn(INVOKESPECIAL, interName, "<init>", Type.getConstructorDescriptor(constructor), false);
            mv.visitInsn(ARETURN);
            mv.visitMaxs((constructorParameters.length > 0 ? (constructorParameters.length + 3) : 2), 2);
            mv.visitEnd();
        }
        { //虚拟 create 方法
            mv = cw.visitMethod(ACC_PUBLIC + ACC_BRIDGE + ACC_VARARGS + ACC_SYNTHETIC, "create", "([Ljava/lang/Object;)Ljava/lang/Object;", null, null);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitMethodInsn(INVOKEVIRTUAL, newDynName, "create", "([Ljava/lang/Object;)" + interDesc, false);
            mv.visitInsn(ARETURN);
            mv.visitMaxs(2, 2);
            mv.visitEnd();
        }
        cw.visitEnd();
        final byte[] bytes = cw.toByteArray();
        final boolean ispub = Modifier.isPublic(constructor.getModifiers());
        Class<?> resultClazz = null;
        if (loader instanceof URLClassLoader && !ispub) {
            try {
                final URLClassLoader urlLoader = (URLClassLoader) loader;
                final URL url = new URL("memclass", "localhost", -1, "/" + newDynName.replace('/', '.') + "/", new URLStreamHandler() {
                    @Override
                    protected URLConnection openConnection(URL u) throws IOException {
                        return new URLConnection(u) {
                            @Override
                            public void connect() throws IOException {
                            }

                            @Override
                            public InputStream getInputStream() throws IOException {
                                return new ByteArrayInputStream(bytes);
                            }
                        };
                    }
                });
                Method addURLMethod = URLClassLoader.class.getDeclaredMethod("addURL", URL.class);
                addURLMethod.setAccessible(true);
                addURLMethod.invoke(urlLoader, url);
                resultClazz = urlLoader.loadClass(newDynName.replace('/', '.'));
            } catch (Throwable t) { //异常无需理会， 使用下一种loader方式
                t.printStackTrace();
            }
        }
        if (!ispub && resultClazz == null) throw new RuntimeException("[" + clazz + "] have no public or ConstructorParameters-Annotation constructor.");
        try {
            if (resultClazz == null) resultClazz = new ClassLoader(loader) {
                    public final Class<?> loadClass(String name, byte[] b) {
                        return defineClass(name, b, 0, b.length);
                    }
                }.loadClass(newDynName.replace('/', '.'), bytes);
            return (Creator) resultClazz.getDeclaredConstructor().newInstance();
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }
}
