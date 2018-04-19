/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import org.redkale.asm.MethodDebugVisitor;
import java.io.*;
import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.reflect.*;
import java.nio.channels.CompletionHandler;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import javax.annotation.Resource;
import org.redkale.asm.*;
import static org.redkale.asm.ClassWriter.COMPUTE_FRAMES;
import static org.redkale.asm.Opcodes.*;
import org.redkale.asm.Type;
import org.redkale.convert.*;
import org.redkale.convert.json.*;
import org.redkale.net.Cryptor;
import org.redkale.service.*;
import org.redkale.util.*;
import org.redkale.source.Flipper;

/**
 * 以find开头的方法且参数只有一个且参数类型为primitive class或String，则RestParam值默认为#
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@SuppressWarnings("unchecked")
public final class Rest {

    public static final String REST_HEADER_RESOURCE_NAME = "rest-resource-name";

    static final String REST_SERVICE_FIELD_NAME = "_redkale_service";

    static final String REST_JSONCONVERT_FIELD_PREFIX = "_redkale_jsonconvert_";

    static final String REST_SERVICEMAP_FIELD_NAME = "_redkale_servicemap"; //如果只有name=""的Service资源，则实例中_servicemap必须为null

    private static final String REST_PARAMTYPES_FIELD_NAME = "_redkale_paramtypes"; //存在泛型的参数数组 Type[][] 第1维度是方法的下标， 第二维度是参数的下标

    private static final Set<String> EXCLUDERMETHODS = new HashSet<>();

    static {
        for (Method m : Object.class.getMethods()) {
            EXCLUDERMETHODS.add(m.getName());
        }
    }

    /**
     * 用于标记由Rest.createRestServlet 方法创建的RestServlet
     */
    @Inherited
    @Documented
    @Target({TYPE})
    @Retention(RUNTIME)
    public static @interface RestDyn {

    }

    /**
     * 用于标记由Rest.createRestServlet 方法创建的RestServlet
     */
    @Inherited
    @Documented
    @Target({TYPE})
    @Retention(RUNTIME)
    public static @interface RestDynSourceType {

        Class value();
    }

    private Rest() {
    }

    static class MethodParamClassVisitor extends ClassVisitor {

        private final Map<String, List<String>> fieldmap;

        public MethodParamClassVisitor(int api, final Map<String, List<String>> fieldmap) {
            super(api);
            this.fieldmap = fieldmap;
        }

        @Override
        public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
            if (java.lang.reflect.Modifier.isStatic(access)) return null;
            List<String> fieldnames = new ArrayList<>();
            String key = name + ":" + desc;
            if (fieldmap.containsKey(key)) return null;
            fieldmap.put(key, fieldnames);
            return new MethodVisitor(Opcodes.ASM6) {
                @Override
                public void visitLocalVariable(String name, String description, String signature, Label start, Label end, int index) {
                    if (index < 1) return;
                    int size = fieldnames.size();
                    //index并不会按顺序执行的
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

        //返回的List中参数列表可能会比方法参数量多，因为方法内的临时变量也会存入list中， 所以需要list的元素集合比方法的参数多
        public static Map<String, List<String>> getMethodParamNames(Map<String, List<String>> map, Class clazz) {
            String n = clazz.getName();
            InputStream in = clazz.getResourceAsStream(n.substring(n.lastIndexOf('.') + 1) + ".class");
            if (in == null) return map;
            try {
                new ClassReader(Utility.readBytesThenClose(in)).accept(new MethodParamClassVisitor(Opcodes.ASM6, map), 0);
            } catch (Exception e) { //无需理会 
            }
            Class superClass = clazz.getSuperclass();
            if (superClass == Object.class) return map;
            return getMethodParamNames(map, superClass);
        }
    }

    static JsonConvert createJsonConvert(RestConvert[] converts) {
        if (converts == null || converts.length < 1) return JsonConvert.root();
        final JsonFactory childFactory = JsonFactory.create();
        List<Class> types = new ArrayList<>();
        for (RestConvert rc : converts) {
            if (types.contains(rc.type())) throw new RuntimeException("@RestConvert type(" + rc.type() + ") repeat");
            if (rc.skipIgnore()) {
                childFactory.registerSkipIgnore(rc.type());
                childFactory.reloadCoder(rc.type());
            } else {
                childFactory.register(rc.type(), false, rc.convertColumns());
                childFactory.register(rc.type(), true, rc.ignoreColumns());
                childFactory.reloadCoder(rc.type());
            }
            types.add(rc.type());
            childFactory.tiny(rc.tiny());
        }
        return childFactory.getConvert();
    }

    static String getWebModuleNameLowerCase(Class<? extends Service> serviceType) {
        final RestService controller = serviceType.getAnnotation(RestService.class);
        if (controller == null) return serviceType.getSimpleName().replaceAll("Service.*$", "").toLowerCase();
        if (controller.ignore()) return null;
        return (!controller.name().isEmpty()) ? controller.name().trim() : serviceType.getSimpleName().replaceAll("Service.*$", "").toLowerCase();
    }

    static String getWebModuleName(Class<? extends Service> serviceType) {
        final RestService controller = serviceType.getAnnotation(RestService.class);
        if (controller == null) return serviceType.getSimpleName().replaceAll("Service.*$", "");
        if (controller.ignore()) return null;
        return (!controller.name().isEmpty()) ? controller.name().trim() : serviceType.getSimpleName().replaceAll("Service.*$", "");
    }

    static boolean isRestDyn(HttpServlet servlet) {
        return servlet.getClass().getAnnotation(RestDyn.class) != null;
    }

    static Service getService(HttpServlet servlet) {
        if (servlet == null) return null;
        try {
            Field ts = servlet.getClass().getDeclaredField(REST_SERVICE_FIELD_NAME);
            ts.setAccessible(true);
            return (Service) ts.get(servlet);
        } catch (Exception e) {
            return null;
        }
    }

    static Map<String, Service> getServiceMap(HttpServlet servlet) {
        if (servlet == null) return null;
        try {
            Field ts = servlet.getClass().getDeclaredField(REST_SERVICEMAP_FIELD_NAME);
            ts.setAccessible(true);
            return (Map) ts.get(servlet);
        } catch (Exception e) {
            return null;
        }
    }

    public static <T extends HttpServlet> T createRestWebSocketServlet(final ClassLoader classLoader, final Class<? extends WebSocket> webSocketType) {
        if (webSocketType == null) throw new RuntimeException("Rest WebSocket Class is null on createRestWebSocketServlet");
        if (Modifier.isAbstract(webSocketType.getModifiers())) throw new RuntimeException("Rest WebSocket Class(" + webSocketType + ") cannot abstract on createRestWebSocketServlet");
        if (Modifier.isFinal(webSocketType.getModifiers())) throw new RuntimeException("Rest WebSocket Class(" + webSocketType + ") cannot final on createRestWebSocketServlet");
        final RestWebSocket rws = webSocketType.getAnnotation(RestWebSocket.class);
        if (rws == null || rws.ignore()) throw new RuntimeException("Rest WebSocket Class(" + webSocketType + ") have not @RestWebSocket or @RestWebSocket.ignore=true on createRestWebSocketServlet");
        boolean valid = false;
        for (Constructor c : webSocketType.getDeclaredConstructors()) {
            if (c.getParameterCount() == 0 && (Modifier.isPublic(c.getModifiers()) || Modifier.isProtected(c.getModifiers()))) {
                valid = true;
                break;
            }
        }
        if (!valid) throw new RuntimeException("Rest WebSocket Class(" + webSocketType + ") must have public or protected Constructor on createRestWebSocketServlet");
        final String rwsname = ResourceFactory.formatResourceName(rws.name());
        if (!checkName(rws.catalog())) throw new RuntimeException(webSocketType.getName() + " have illegal " + RestWebSocket.class.getSimpleName() + ".catalog, only 0-9 a-z A-Z _ cannot begin 0-9");
        if (!checkName(rwsname)) throw new RuntimeException(webSocketType.getName() + " have illegal " + RestWebSocket.class.getSimpleName() + ".name, only 0-9 a-z A-Z _ cannot begin 0-9");

        //----------------------------------------------------------------------------------------
        final Set<Field> resourcesFieldSet = new LinkedHashSet<>();
        final ClassLoader loader = classLoader == null ? Thread.currentThread().getContextClassLoader() : classLoader;
        final Set<String> resourcesFieldNameSet = new HashSet<>();
        Class clzz = webSocketType;
        do {
            for (Field field : clzz.getDeclaredFields()) {
                if (field.getAnnotation(Resource.class) == null) continue;
                if (resourcesFieldNameSet.contains(field.getName())) continue;
                if (Modifier.isStatic(field.getModifiers())) throw new RuntimeException(field + " cannot static on createRestWebSocketServlet");
                if (Modifier.isFinal(field.getModifiers())) throw new RuntimeException(field + " cannot final on createRestWebSocketServlet");
                if (!Modifier.isPublic(field.getModifiers()) && !Modifier.isProtected(field.getModifiers())) {
                    throw new RuntimeException(field + " must be public or protected on createRestWebSocketServlet");
                }
                resourcesFieldNameSet.add(field.getName());
                resourcesFieldSet.add(field);
            }
        } while ((clzz = clzz.getSuperclass()) != Object.class);

        final List<Field> resourcesFields = new ArrayList<>(resourcesFieldSet);
        StringBuilder sb1 = new StringBuilder();
        StringBuilder sb2 = new StringBuilder();
        for (int i = 0; i < resourcesFields.size(); i++) {
            Field field = resourcesFields.get(i);
            sb1.append(Type.getDescriptor(field.getType()));
            sb2.append(Utility.getTypeDescriptor(field.getGenericType()));
        }
        final String resourceDescriptor = sb1.toString();
        final String resourceGenericDescriptor = sb1.length() == sb2.length() ? null : sb2.toString();

        //----------------------------------------------------------------------------------------        
        final Map<String, List<String>> asmParamMap = MethodParamClassVisitor.getMethodParamNames(new HashMap<>(), webSocketType);
        final Set<String> messageNames = new HashSet<>();
        final List<Method> messageMethods = new ArrayList<>();
        for (Method method : webSocketType.getMethods()) {
            RestOnMessage rom = method.getAnnotation(RestOnMessage.class);
            if (rom == null) continue;
            String name = rom.name();
            if (Modifier.isFinal(method.getModifiers())) throw new RuntimeException("@RestOnMessage method can not final but (" + method + ")");
            if (Modifier.isStatic(method.getModifiers())) throw new RuntimeException("@RestOnMessage method can not static but (" + method + ")");
            if (method.getReturnType() != void.class) throw new RuntimeException("@RestOnMessage method must return void but (" + method + ")");
            if (method.getExceptionTypes().length > 0) throw new RuntimeException("@RestOnMessage method can not throw exception but (" + method + ")");
            if (name.isEmpty()) throw new RuntimeException(method + " RestOnMessage.name is empty createRestWebSocketServlet");
            if (messageNames.contains(name)) throw new RuntimeException(method + " repeat RestOnMessage.name(" + name + ") createRestWebSocketServlet");
            messageNames.add(name);
            messageMethods.add(method);
        }
        //----------------------------------------------------------------------------------------
        final String resDesc = Type.getDescriptor(Resource.class);
        final String wsDesc = Type.getDescriptor(WebSocket.class);
        final String wsParamDesc = Type.getDescriptor(WebSocketParam.class);
        final String jsonConvertDesc = Type.getDescriptor(JsonConvert.class);
        final String convertDisabledDesc = Type.getDescriptor(ConvertDisabled.class);
        final String webSocketParamName = Type.getInternalName(WebSocketParam.class);
        final String supDynName = WebSocketServlet.class.getName().replace('.', '/');
        final String webServletDesc = Type.getDescriptor(WebServlet.class);
        final String webSocketInternalName = Type.getInternalName(webSocketType);

        final String newDynName = webSocketInternalName.substring(0, webSocketInternalName.lastIndexOf('/') + 1) + "_Dyn" + webSocketType.getSimpleName() + "Servlet";

        final String newDynWebSokcetSimpleName = "_Dyn" + webSocketType.getSimpleName();
        final String newDynWebSokcetFullName = newDynName + "$" + newDynWebSokcetSimpleName;

        final String newDynMessageSimpleName = "_Dyn" + webSocketType.getSimpleName() + "Message";
        final String newDynMessageFullName = newDynName + "$" + newDynMessageSimpleName;

        final String newDynConsumerSimpleName = "_DynRestOnMessageConsumer";
        final String newDynConsumerFullName = newDynName + "$" + newDynConsumerSimpleName;
        //----------------------------------------------------------------------------------------

        ClassWriter cw = new ClassWriter(COMPUTE_FRAMES);
        FieldVisitor fv;
        MethodDebugVisitor mv;
        AnnotationVisitor av0;
        cw.visit(V1_8, ACC_PUBLIC + ACC_FINAL + ACC_SUPER, newDynName, null, supDynName, null);

        { //RestDynSourceType
            av0 = cw.visitAnnotation(Type.getDescriptor(RestDynSourceType.class), true);
            av0.visit("value", Type.getType(Type.getDescriptor(webSocketType)));
            av0.visitEnd();
        }
        { //注入 @WebServlet 注解
            String urlpath = (rws.catalog().isEmpty() ? "/" : ("/" + rws.catalog() + "/")) + rwsname;
            av0 = cw.visitAnnotation(webServletDesc, true);
            {
                AnnotationVisitor av1 = av0.visitArray("value");
                av1.visit(null, urlpath);
                av1.visitEnd();
            }
            av0.visit("moduleid", 0);
            av0.visit("repair", rws.repair());
            av0.visit("comment", rws.comment());
            av0.visitEnd();
        }
        { //内部类
            cw.visitInnerClass(newDynConsumerFullName, newDynName, newDynConsumerSimpleName, ACC_PUBLIC + ACC_STATIC);

            cw.visitInnerClass(newDynWebSokcetFullName, newDynName, newDynWebSokcetSimpleName, ACC_PUBLIC + ACC_STATIC);

            cw.visitInnerClass(newDynMessageFullName, newDynName, newDynMessageSimpleName, ACC_PUBLIC + ACC_STATIC);

            for (int i = 0; i < messageMethods.size(); i++) {
                Method method = messageMethods.get(i);
                String endfix = "_" + method.getName() + "_" + (i > 9 ? i : ("0" + i));
                cw.visitInnerClass(newDynMessageFullName + endfix, newDynName, newDynMessageSimpleName + endfix, ACC_PUBLIC + ACC_STATIC);
            }
        }
        { //@Resource
            for (int i = 0; i < resourcesFields.size(); i++) {
                Field field = resourcesFields.get(i);
                Resource res = field.getAnnotation(Resource.class);
                java.lang.reflect.Type fieldType = field.getGenericType();
                fv = cw.visitField(ACC_PRIVATE, "_redkale_resource_" + i, Type.getDescriptor(field.getType()), fieldType == field.getType() ? null : Utility.getTypeDescriptor(fieldType), null);
                {
                    av0 = fv.visitAnnotation(resDesc, true);
                    av0.visit("name", res.name());
                    av0.visitEnd();
                }
                fv.visitEnd();
            }
        }
        { //_DynWebSocketServlet构造函数 
            mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null));
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, supDynName, "<init>", "()V", false);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitLdcInsn(Type.getObjectType(newDynName + "$" + newDynWebSokcetSimpleName + "Message"));
            mv.visitFieldInsn(PUTFIELD, newDynName, "messageTextType", "Ljava/lang/reflect/Type;");

            mv.visitVarInsn(ALOAD, 0);
            pushInt(mv, rws.liveinterval());
            mv.visitFieldInsn(PUTFIELD, newDynName, "liveinterval", "I");

            mv.visitVarInsn(ALOAD, 0);
            pushInt(mv, rws.wsmaxconns());
            mv.visitFieldInsn(PUTFIELD, newDynName, "wsmaxconns", "I");

            mv.visitVarInsn(ALOAD, 0);
            pushInt(mv, rws.wsmaxbody());
            mv.visitFieldInsn(PUTFIELD, newDynName, "wsmaxbody", "I");

            mv.visitVarInsn(ALOAD, 0);
            mv.visitInsn(rws.single() ? ICONST_1 : ICONST_0);
            mv.visitFieldInsn(PUTFIELD, newDynName, "single", "Z");

            mv.visitVarInsn(ALOAD, 0);
            mv.visitInsn(rws.anyuser() ? ICONST_1 : ICONST_0);
            mv.visitFieldInsn(PUTFIELD, newDynName, "anyuser", "Z");

            mv.visitInsn(RETURN);
            mv.visitMaxs(3, 1);
            mv.visitEnd();
        }
        { //createWebSocket 方法
            mv = new MethodDebugVisitor(cw.visitMethod(ACC_PROTECTED, "createWebSocket", "()" + wsDesc, "<G::Ljava/io/Serializable;T:Ljava/lang/Object;>()L" + WebSocket.class.getName().replace('.', '/') + "<TG;TT;>;", null));
            mv.visitTypeInsn(NEW, newDynName + "$" + newDynWebSokcetSimpleName);
            mv.visitInsn(DUP);
            for (int i = 0; i < resourcesFields.size(); i++) {
                mv.visitVarInsn(ALOAD, 0);
                mv.visitFieldInsn(GETFIELD, newDynName, "_redkale_resource_" + i, Type.getDescriptor(resourcesFields.get(i).getType()));
            }
            mv.visitMethodInsn(INVOKESPECIAL, newDynWebSokcetFullName, "<init>", "(" + resourceDescriptor + ")V", false);
            mv.visitInsn(ARETURN);
            mv.visitMaxs(2 + resourcesFields.size(), 1);
            mv.visitEnd();
        }
        { //createRestOnMessageConsumer
            mv = new MethodDebugVisitor(cw.visitMethod(ACC_PROTECTED, "createRestOnMessageConsumer", "()Ljava/util/function/BiConsumer;", "()Ljava/util/function/BiConsumer<" + wsDesc + "Ljava/lang/Object;>;", null));
            mv.visitTypeInsn(NEW, newDynConsumerFullName);
            mv.visitInsn(DUP);
            mv.visitMethodInsn(INVOKESPECIAL, newDynConsumerFullName, "<init>", "()V", false);
            mv.visitInsn(ARETURN);
            mv.visitMaxs(2, 1);
            mv.visitEnd();
        }
        { //resourceName
            mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "resourceName", "()Ljava/lang/String;", null, null));
            mv.visitLdcInsn(rwsname);
            mv.visitInsn(ARETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }

        RestClassLoader newLoader = new RestClassLoader(loader);

        for (int i = 0; i < messageMethods.size(); i++) {  // _DyncXXXWebSocketMessage 子消息List
            Method method = messageMethods.get(i);
            String endfix = "_" + method.getName() + "_" + (i > 9 ? i : ("0" + i));

            ClassWriter cw2 = new ClassWriter(COMPUTE_FRAMES);
            cw2.visit(V1_8, ACC_PUBLIC + ACC_FINAL + ACC_SUPER, newDynMessageFullName + endfix, null, "java/lang/Object", new String[]{webSocketParamName, "java/lang/Runnable"});
            cw2.visitInnerClass(newDynMessageFullName + endfix, newDynName, newDynMessageSimpleName + endfix, ACC_PUBLIC + ACC_STATIC);
            Set<String> paramnames = new HashSet<>();
            String methodesc = method.getName() + ":" + Type.getMethodDescriptor(method);
            List<String> names = asmParamMap.get(methodesc);
            Parameter[] params = method.getParameters();
            final LinkedHashMap<String, Parameter> paramap = new LinkedHashMap(); //必须使用LinkedHashMap确保顺序
            for (int j = 0; j < params.length; j++) { //字段列表
                Parameter param = params[j];
                String paramname = param.getName();
                RestParam rp = param.getAnnotation(RestParam.class);
                if (rp != null && !rp.name().isEmpty()) {
                    paramname = rp.name();
                } else if (names != null && names.size() > j) {
                    paramname = names.get(j);
                }
                if (paramnames.contains(paramname)) throw new RuntimeException(method + " has same @RestParam.name");
                paramnames.add(paramname);
                paramap.put(paramname, param);
                fv = cw2.visitField(ACC_PUBLIC, paramname, Type.getDescriptor(param.getType()),
                    param.getType() == param.getParameterizedType() ? null : Utility.getTypeDescriptor(param.getParameterizedType()), null);
                fv.visitEnd();
            }
            { //_redkale_websocket
                fv = cw2.visitField(ACC_PUBLIC, "_redkale_websocket", "L" + newDynWebSokcetFullName + ";", null, null);
                av0 = fv.visitAnnotation(convertDisabledDesc, true);
                av0.visitEnd();
                fv.visitEnd();
            }
            { //空构造函数
                mv = new MethodDebugVisitor(cw2.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null));
                mv.visitVarInsn(ALOAD, 0);
                mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
                mv.visitInsn(RETURN);
                mv.visitMaxs(1, 1);
                mv.visitEnd();
            }
            { //getNames
                mv = new MethodDebugVisitor(cw2.visitMethod(ACC_PUBLIC, "getNames", "()[Ljava/lang/String;", null, null));
                pushInt(mv, paramap.size());
                mv.visitTypeInsn(ANEWARRAY, "java/lang/String");
                int index = -1;
                for (Map.Entry<String, Parameter> en : paramap.entrySet()) {
                    mv.visitInsn(DUP);
                    pushInt(mv, ++index);
                    mv.visitLdcInsn(en.getKey());
                    mv.visitInsn(AASTORE);
                }
                mv.visitInsn(ARETURN);
                mv.visitMaxs(paramap.size() + 2, 1);
                mv.visitEnd();
            }
            { //getValue
                mv = new MethodDebugVisitor(cw2.visitMethod(ACC_PUBLIC, "getValue", "(Ljava/lang/String;)Ljava/lang/Object;", "<T:Ljava/lang/Object;>(Ljava/lang/String;)TT;", null));
                for (Map.Entry<String, Parameter> en : paramap.entrySet()) {
                    Class paramType = en.getValue().getType();
                    mv.visitLdcInsn(en.getKey());
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "equals", "(Ljava/lang/Object;)Z", false);
                    Label l1 = new Label();
                    mv.visitJumpInsn(IFEQ, l1);
                    mv.visitVarInsn(ALOAD, 0);
                    mv.visitFieldInsn(GETFIELD, newDynMessageFullName + endfix, en.getKey(), Type.getDescriptor(paramType));
                    if (paramType.isPrimitive()) {
                        Class bigclaz = java.lang.reflect.Array.get(java.lang.reflect.Array.newInstance(paramType, 1), 0).getClass();
                        mv.visitMethodInsn(INVOKESTATIC, bigclaz.getName().replace('.', '/'), "valueOf", "(" + Type.getDescriptor(paramType) + ")" + Type.getDescriptor(bigclaz), false);
                    }
                    mv.visitInsn(ARETURN);
                    mv.visitLabel(l1);
                }
                mv.visitInsn(ACONST_NULL);
                mv.visitInsn(ARETURN);
                mv.visitMaxs(2, 2);
                mv.visitEnd();
            }
            { //execute
                mv = new MethodDebugVisitor(cw2.visitMethod(ACC_PUBLIC, "execute", "(L" + newDynWebSokcetFullName + ";)V", null, null));
                mv.visitVarInsn(ALOAD, 0);
                mv.visitVarInsn(ALOAD, 1);
                mv.visitFieldInsn(PUTFIELD, newDynMessageFullName + endfix, "_redkale_websocket", "L" + newDynWebSokcetFullName + ";");
                mv.visitVarInsn(ALOAD, 1);
                mv.visitLdcInsn(method.getAnnotation(RestOnMessage.class).name());
                mv.visitVarInsn(ALOAD, 0);
                mv.visitVarInsn(ALOAD, 0);
                mv.visitMethodInsn(INVOKEVIRTUAL, newDynWebSokcetFullName, "preOnMessage", "(Ljava/lang/String;" + wsParamDesc + "Ljava/lang/Runnable;)V", false);
                mv.visitInsn(RETURN);
                mv.visitMaxs(4, 2);
                mv.visitEnd();
            }
            { //run
                mv = new MethodDebugVisitor(cw2.visitMethod(ACC_PUBLIC, "run", "()V", null, null));
                mv.visitVarInsn(ALOAD, 0);
                mv.visitFieldInsn(GETFIELD, newDynMessageFullName + endfix, "_redkale_websocket", "L" + newDynWebSokcetFullName + ";");

                for (Map.Entry<String, Parameter> en : paramap.entrySet()) {
                    mv.visitVarInsn(ALOAD, 0);
                    mv.visitFieldInsn(GETFIELD, (newDynMessageFullName + endfix), en.getKey(), Type.getDescriptor(en.getValue().getType()));
                }
                mv.visitMethodInsn(INVOKEVIRTUAL, newDynWebSokcetFullName, method.getName(), Type.getMethodDescriptor(method), false);

                mv.visitInsn(RETURN);
                mv.visitMaxs(3, 1);
                mv.visitEnd();
            }
            { //toString
                mv = new MethodDebugVisitor(cw2.visitMethod(ACC_PUBLIC, "toString", "()Ljava/lang/String;", null, null));
                mv.visitMethodInsn(INVOKESTATIC, JsonConvert.class.getName().replace('.', '/'), "root", "()" + jsonConvertDesc, false);
                mv.visitVarInsn(ALOAD, 0);
                mv.visitMethodInsn(INVOKEVIRTUAL, JsonConvert.class.getName().replace('.', '/'), "convertTo", "(Ljava/lang/Object;)Ljava/lang/String;", false);
                mv.visitInsn(ARETURN);
                mv.visitMaxs(2, 1);
                mv.visitEnd();
            }
            cw2.visitEnd();
            newLoader.loadClass((newDynMessageFullName + endfix).replace('/', '.'), cw2.toByteArray());
        }

        { //_DynXXXWebSocketMessage class
            ClassWriter cw2 = new ClassWriter(COMPUTE_FRAMES);
            cw2.visit(V1_8, ACC_PUBLIC + ACC_FINAL + ACC_SUPER, newDynMessageFullName, null, "java/lang/Object", null);

            cw2.visitInnerClass(newDynMessageFullName, newDynName, newDynMessageSimpleName, ACC_PUBLIC + ACC_STATIC);

            for (int i = 0; i < messageMethods.size(); i++) {
                Method method = messageMethods.get(i);
                String endfix = "_" + method.getName() + "_" + (i > 9 ? i : ("0" + i));
                cw2.visitInnerClass(newDynMessageFullName + endfix, newDynName, newDynMessageSimpleName + endfix, ACC_PUBLIC + ACC_STATIC);

                fv = cw2.visitField(ACC_PUBLIC, method.getAnnotation(RestOnMessage.class).name(), "L" + newDynMessageFullName + endfix + ";", null, null);
                fv.visitEnd();
            }
            { //构造函数
                mv = new MethodDebugVisitor(cw2.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null));
                mv.visitVarInsn(ALOAD, 0);
                mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
                mv.visitInsn(RETURN);
                mv.visitMaxs(1, 1);
                mv.visitEnd();
            }
            { //toString
                mv = new MethodDebugVisitor(cw2.visitMethod(ACC_PUBLIC, "toString", "()Ljava/lang/String;", null, null));
                mv.visitMethodInsn(INVOKESTATIC, JsonConvert.class.getName().replace('.', '/'), "root", "()" + jsonConvertDesc, false);
                mv.visitVarInsn(ALOAD, 0);
                mv.visitMethodInsn(INVOKEVIRTUAL, JsonConvert.class.getName().replace('.', '/'), "convertTo", "(Ljava/lang/Object;)Ljava/lang/String;", false);
                mv.visitInsn(ARETURN);
                mv.visitMaxs(2, 1);
                mv.visitEnd();
            }
            cw2.visitEnd();
            newLoader.loadClass(newDynMessageFullName.replace('/', '.'), cw2.toByteArray());
        }

        { //_DynXXXWebSocket class
            ClassWriter cw2 = new ClassWriter(COMPUTE_FRAMES);
            cw2.visit(V1_8, ACC_PUBLIC + ACC_FINAL + ACC_SUPER, newDynWebSokcetFullName, null, webSocketInternalName, null);

            cw2.visitInnerClass(newDynWebSokcetFullName, newDynName, newDynWebSokcetSimpleName, ACC_PUBLIC + ACC_STATIC);

            {
                mv = new MethodDebugVisitor(cw2.visitMethod(ACC_PUBLIC, "<init>", "(" + resourceDescriptor + ")V", resourceGenericDescriptor == null ? null : ("(" + resourceGenericDescriptor + ")V"), null));
                mv.visitVarInsn(ALOAD, 0);
                mv.visitMethodInsn(INVOKESPECIAL, webSocketInternalName, "<init>", "()V", false);
                for (int i = 0; i < resourcesFields.size(); i++) {
                    Field field = resourcesFields.get(i);
                    mv.visitVarInsn(ALOAD, 0);
                    mv.visitVarInsn(ALOAD, i + 1);
                    mv.visitFieldInsn(PUTFIELD, newDynWebSokcetFullName, field.getName(), Type.getDescriptor(field.getType()));
                }
                mv.visitInsn(RETURN);
                mv.visitMaxs(2, 1 + resourcesFields.size());
                mv.visitEnd();
            }
            cw2.visitEnd();
            newLoader.loadClass(newDynWebSokcetFullName.replace('/', '.'), cw2.toByteArray());
        }

        { //_DynRestOnMessageConsumer class
            ClassWriter cw2 = new ClassWriter(COMPUTE_FRAMES);
            cw2.visit(V1_8, ACC_PUBLIC + ACC_FINAL + ACC_SUPER, newDynConsumerFullName, "Ljava/lang/Object;Ljava/util/function/BiConsumer<" + wsDesc + "Ljava/lang/Object;>;", "java/lang/Object", new String[]{"java/util/function/BiConsumer"});

            cw2.visitInnerClass(newDynConsumerFullName, newDynName, newDynConsumerSimpleName, ACC_PUBLIC + ACC_STATIC);
            cw2.visitInnerClass(newDynMessageFullName, newDynName, newDynMessageSimpleName, ACC_PUBLIC + ACC_STATIC);
            for (int i = 0; i < messageMethods.size(); i++) {
                Method method = messageMethods.get(i);
                String endfix = "_" + method.getName() + "_" + (i > 9 ? i : ("0" + i));
                cw2.visitInnerClass(newDynMessageFullName + endfix, newDynName, newDynMessageSimpleName + endfix, ACC_PUBLIC + ACC_STATIC);
            }

            { //构造函数
                mv = new MethodDebugVisitor(cw2.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null));
                mv.visitVarInsn(ALOAD, 0);
                mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false);
                mv.visitInsn(RETURN);
                mv.visitMaxs(1, 1);
                mv.visitEnd();
            }

            { //accept函数
                mv = new MethodDebugVisitor(cw2.visitMethod(ACC_PUBLIC, "accept", "(" + wsDesc + "Ljava/lang/Object;)V", null, null));
                mv.visitVarInsn(ALOAD, 1);
                mv.visitTypeInsn(CHECKCAST, newDynWebSokcetFullName);
                mv.visitVarInsn(ASTORE, 3);

                mv.visitVarInsn(ALOAD, 2);
                mv.visitTypeInsn(CHECKCAST, newDynMessageFullName);
                mv.visitVarInsn(ASTORE, 4);

                for (int i = 0; i < messageMethods.size(); i++) {
                    final Method method = messageMethods.get(i);
                    String endfix = "_" + method.getName() + "_" + (i > 9 ? i : ("0" + i));
                    final String messagename = method.getAnnotation(RestOnMessage.class).name();

                    mv.visitVarInsn(ALOAD, 4);
                    mv.visitFieldInsn(GETFIELD, newDynMessageFullName, messagename, "L" + (newDynMessageFullName + endfix) + ";");
                    Label ifLabel = new Label();
                    mv.visitJumpInsn(IFNULL, ifLabel);

                    mv.visitVarInsn(ALOAD, 4);
                    mv.visitFieldInsn(GETFIELD, newDynMessageFullName, messagename, "L" + (newDynMessageFullName + endfix) + ";");
                    mv.visitVarInsn(ALOAD, 3);
                    mv.visitMethodInsn(INVOKEVIRTUAL, (newDynMessageFullName + endfix), "execute", "(L" + newDynWebSokcetFullName + ";)V", false);
                    mv.visitInsn(RETURN);
                    mv.visitLabel(ifLabel);
                }
                mv.visitInsn(RETURN);
                mv.visitMaxs(3, 3 + messageMethods.size());
                mv.visitEnd();
            }
            {//虚拟accept函数
                mv = new MethodDebugVisitor(cw2.visitMethod(ACC_PUBLIC + ACC_BRIDGE + ACC_SYNTHETIC, "accept", "(Ljava/lang/Object;Ljava/lang/Object;)V", null, null));
                mv.visitVarInsn(ALOAD, 0);
                mv.visitVarInsn(ALOAD, 1);
                mv.visitTypeInsn(CHECKCAST, WebSocket.class.getName().replace('.', '/'));
                mv.visitVarInsn(ALOAD, 2);
                mv.visitTypeInsn(CHECKCAST, "java/lang/Object");
                mv.visitMethodInsn(INVOKEVIRTUAL, newDynConsumerFullName, "accept", "(" + wsDesc + "Ljava/lang/Object;)V", false);
                mv.visitInsn(RETURN);
                mv.visitMaxs(3, 3);
                mv.visitEnd();
            }
            cw2.visitEnd();
            newLoader.loadClass(newDynConsumerFullName.replace('/', '.'), cw2.toByteArray());
        }
        cw.visitEnd();
        Class<?> newClazz = newLoader.loadClass(newDynName.replace('/', '.'), cw.toByteArray());
        try {
            T servlet = (T) newClazz.getDeclaredConstructor().newInstance();
            if (rws.cryptor() != Cryptor.class) {
                Cryptor cryptor = rws.cryptor().getDeclaredConstructor().newInstance();
                Field cryptorField = newClazz.getSuperclass().getDeclaredField("cryptor"); //WebSocketServlet
                cryptorField.setAccessible(true);
                cryptorField.set(servlet, cryptor);
            }
            return servlet;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public static <T extends HttpServlet> T createRestServlet(final ClassLoader classLoader, final Class userType0, final Class<T> baseServletType, final Class<? extends Service> serviceType) {
        if (baseServletType == null || serviceType == null) throw new RuntimeException(" Servlet or Service is null Class on createRestServlet");
        if (!HttpServlet.class.isAssignableFrom(baseServletType)) throw new RuntimeException(baseServletType + " is not HttpServlet Class on createRestServlet");
        int mod = baseServletType.getModifiers();
        if (!java.lang.reflect.Modifier.isPublic(mod)) throw new RuntimeException(baseServletType + " is not Public Class on createRestServlet");
        if (java.lang.reflect.Modifier.isAbstract(mod)) throw new RuntimeException(baseServletType + " cannot a abstract Class on createRestServlet");

        final String serviceDesc = Type.getDescriptor(serviceType);
        final String webServletDesc = Type.getDescriptor(WebServlet.class);
        final String resDesc = Type.getDescriptor(Resource.class);
        final String reqDesc = Type.getDescriptor(HttpRequest.class);
        final String respDesc = Type.getDescriptor(HttpResponse.class);
        final String convertDesc = Type.getDescriptor(Convert.class);
        final String typeDesc = Type.getDescriptor(java.lang.reflect.Type.class);
        final String jsonConvertDesc = Type.getDescriptor(JsonConvert.class);
        final String retDesc = Type.getDescriptor(RetResult.class);
        final String futureDesc = Type.getDescriptor(CompletableFuture.class);
        final String flipperDesc = Type.getDescriptor(Flipper.class);
        final String httprsDesc = Type.getDescriptor(HttpResult.class);
        final String attrDesc = Type.getDescriptor(org.redkale.util.Attribute.class);
        final String multiContextDesc = Type.getDescriptor(MultiContext.class);
        final String multiContextName = MultiContext.class.getName().replace('.', '/');
        final String mappingDesc = Type.getDescriptor(HttpMapping.class);
        final String webparamDesc = Type.getDescriptor(HttpParam.class);
        final String webparamsDesc = Type.getDescriptor(HttpParam.HttpParams.class);
        final String sourcetypeDesc = Type.getDescriptor(HttpParam.HttpParamSourceType.class);

        final String reqInternalName = Type.getInternalName(HttpRequest.class);
        final String respInternalName = Type.getInternalName(HttpResponse.class);
        final String attrInternalName = Type.getInternalName(org.redkale.util.Attribute.class);
        final String retInternalName = Type.getInternalName(RetResult.class);
        final String serviceTypeInternalName = Type.getInternalName(serviceType);

        HttpUserType hut = baseServletType.getAnnotation(HttpUserType.class);
        final Class userType = (userType0 == null || userType0 == Object.class) ? (hut == null ? null : hut.value()) : userType0;

        final String supDynName = baseServletType.getName().replace('.', '/');
        final RestService controller = serviceType.getAnnotation(RestService.class);
        if (controller != null && controller.ignore()) throw new RuntimeException(serviceType + " is ignore Rest Service Class"); //标记为ignore=true不创建Servlet
        ClassLoader loader = classLoader == null ? Thread.currentThread().getContextClassLoader() : classLoader;
        String newDynName = serviceTypeInternalName.substring(0, serviceTypeInternalName.lastIndexOf('/') + 1) + "_Dyn" + serviceType.getSimpleName().replaceAll("Service.*$", "") + "RestServlet";

        //------------------------------------------------------------------------------
        final String defmodulename = getWebModuleNameLowerCase(serviceType);
        final String bigmodulename = getWebModuleName(serviceType);
        final String catalog = controller == null ? "" : controller.catalog();
        if (!checkName(catalog)) throw new RuntimeException(serviceType.getName() + " have illegal " + RestService.class.getSimpleName() + ".catalog, only 0-9 a-z A-Z _ cannot begin 0-9");
        if (!checkName(defmodulename)) throw new RuntimeException(serviceType.getName() + " have illegal " + RestService.class.getSimpleName() + ".value, only 0-9 a-z A-Z _ cannot begin 0-9");
        ClassWriter cw = new ClassWriter(COMPUTE_FRAMES);
        FieldVisitor fv;
        MethodDebugVisitor mv;
        AnnotationVisitor av0;
        Map<String, Object> classMap = new LinkedHashMap<>();
        List<Map<String, Object>> mappingMaps = new ArrayList<>();
        cw.visit(V1_8, ACC_PUBLIC + ACC_FINAL + ACC_SUPER, newDynName, null, supDynName, null);

        { //RestDyn
            av0 = cw.visitAnnotation(Type.getDescriptor(RestDyn.class), true);
            av0.visitEnd();
        }
        { //RestDynSourceType
            av0 = cw.visitAnnotation(Type.getDescriptor(RestDynSourceType.class), true);
            av0.visit("value", Type.getType(Type.getDescriptor(serviceType)));
            av0.visitEnd();
        }

        final List<MappingEntry> entrys = new ArrayList<>();
        final Map<String, org.redkale.util.Attribute> restAttributes = new LinkedHashMap<>();
        //获取所有可以转换成HttpMapping的方法
        int methodidex = 0;
        final List<java.lang.reflect.Type[]> paramtypes = new ArrayList<>();
        for (final Method method : serviceType.getMethods()) {
            if (Modifier.isStatic(method.getModifiers())) continue;
            if (method.isSynthetic()) continue;
            if (EXCLUDERMETHODS.contains(method.getName())) continue;
            if ("init".equals(method.getName())) continue;
            if ("destroy".equals(method.getName())) continue;
            if ("version".equals(method.getName())) continue;
            if (controller == null) continue;

            RestMapping[] mappings = method.getAnnotationsByType(RestMapping.class);
            if (!controller.automapping() && mappings.length < 1) continue;
            boolean ignore = false;
            for (RestMapping mapping : mappings) {
                if (mapping.ignore()) {
                    ignore = true;
                    break;
                }
            }
            if (ignore) continue;

            Class[] extypes = method.getExceptionTypes();
            if (extypes.length > 1) {
                if (mappings != null && mappings.length > 0) throw new RuntimeException("@" + RestMapping.class.getSimpleName() + " only for method with throws IOException");
                continue;
            }
            if (extypes.length == 1 && extypes[0] != IOException.class) {
                if (mappings != null && mappings.length > 0) throw new RuntimeException("@" + RestMapping.class.getSimpleName() + " only for method with throws IOException");
                continue;
            }
            paramtypes.add(TypeToken.getGenericType(method.getGenericParameterTypes(), serviceType));
            if (mappings.length == 0) { //没有Mapping，设置一个默认值
                MappingEntry entry = new MappingEntry(methodidex, null, bigmodulename, method);
                if (entrys.contains(entry)) throw new RuntimeException(serviceType.getName() + " on " + method.getName() + " 's mapping(" + entry.name + ") is repeat");
                entrys.add(entry);
            } else {
                for (RestMapping mapping : mappings) {
                    MappingEntry entry = new MappingEntry(methodidex, mapping, defmodulename, method);
                    if (entrys.contains(entry)) throw new RuntimeException(serviceType.getName() + " on " + method.getName() + " 's mapping(" + entry.name + ") is repeat");
                    entrys.add(entry);
                }
            }
            methodidex++;
        }
        if (entrys.isEmpty()) return null; //没有可HttpMapping的方法

        { //注入 @WebServlet 注解
            String urlpath = "";
            int moduleid = controller == null ? 0 : controller.moduleid();
            boolean repair = controller == null ? true : controller.repair();
            String comment = controller == null ? "" : controller.comment();
            av0 = cw.visitAnnotation(webServletDesc, true);
            {
                AnnotationVisitor av1 = av0.visitArray("value");
                if (defmodulename.isEmpty()) {
                    for (MappingEntry entry : entrys) {
                        String suburl = (catalog.isEmpty() ? "/" : ("/" + catalog + "/")) + (defmodulename.isEmpty() ? "" : (defmodulename + "/")) + entry.name;
                        urlpath += "," + suburl;
                        av1.visit(null, suburl);
                    }
                    if (urlpath.length() > 0) urlpath = urlpath.substring(1);
                } else {
                    urlpath = (catalog.isEmpty() ? "/" : ("/" + catalog + "/")) + defmodulename + "/*";
                    av1.visit(null, urlpath);
                }
                av1.visitEnd();
            }
            av0.visit("moduleid", moduleid);
            av0.visit("repair", repair);
            av0.visit("comment", comment);
            av0.visitEnd();
            classMap.put("type", serviceType.getName());
            classMap.put("url", urlpath);
            classMap.put("moduleid", moduleid);
            classMap.put("repair", repair);
            //classMap.put("comment", comment); //不显示太多信息
        }

        {  //注入 @Resource  private XXXService _service;
            fv = cw.visitField(ACC_PRIVATE, REST_SERVICE_FIELD_NAME, serviceDesc, null, null);
            av0 = fv.visitAnnotation(resDesc, true);
            av0.visit("name", "");
            av0.visitEnd();
            fv.visitEnd();
        }
        {  //注入 @Resource(name = "APP_HOME")  private File _redkale_home;
            fv = cw.visitField(ACC_PRIVATE, "_redkale_home", Type.getDescriptor(File.class), null, null);
            av0 = fv.visitAnnotation(resDesc, true);
            av0.visit("name", "APP_HOME");
            av0.visitEnd();
            fv.visitEnd();
        }
        { //_servicemap字段 Map<String, XXXService>
            fv = cw.visitField(ACC_PRIVATE, REST_SERVICEMAP_FIELD_NAME, "Ljava/util/Map;", "Ljava/util/Map<Ljava/lang/String;" + serviceDesc + ">;", null);
            fv.visitEnd();
        }
        { //_paramtypes字段 java.lang.reflect.Type[][]
            fv = cw.visitField(ACC_PRIVATE, REST_PARAMTYPES_FIELD_NAME, "[[Ljava/lang/reflect/Type;", null, null);
            fv.visitEnd();
        }
        { //构造函数
            mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null));
            //mv.setDebug(true);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, supDynName, "<init>", "()V", false);
            mv.visitInsn(RETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }

        //将每个Service可转换的方法生成HttpServlet对应的HttpMapping方法
        final Map<String, List<String>> asmParamMap = MethodParamClassVisitor.getMethodParamNames(new HashMap<>(), serviceType);
        final Map<String, java.lang.reflect.Type> bodyTypes = new HashMap<>();

        final List<RestConvert[]> restConverts = new ArrayList<>();
        for (final MappingEntry entry : entrys) {
            RestUploadFile mupload = null;
            Class muploadType = null;
            final Method method = entry.mappingMethod;
            final Class returnType = method.getReturnType();
            final String methodDesc = Type.getMethodDescriptor(method);
            final Parameter[] params = method.getParameters();

            final RestConvert[] rcs = method.getAnnotationsByType(RestConvert.class);
            if (rcs != null && rcs.length > 0) restConverts.add(rcs);

            mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, entry.name, "(" + reqDesc + respDesc + ")V", null, new String[]{"java/io/IOException"}));
            //mv.setDebug(true);
            mv.debugLine();

            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, newDynName, REST_SERVICEMAP_FIELD_NAME, "Ljava/util/Map;");
            Label lmapif = new Label();
            mv.visitJumpInsn(IFNONNULL, lmapif);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, newDynName, REST_SERVICE_FIELD_NAME, serviceDesc);
            Label lserif = new Label();
            mv.visitJumpInsn(GOTO, lserif);
            mv.visitLabel(lmapif);

            mv.visitFrame(Opcodes.F_SAME, 0, null, 0, null);

            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, newDynName, REST_SERVICEMAP_FIELD_NAME, "Ljava/util/Map;");
            mv.visitVarInsn(ALOAD, 1);
            mv.visitLdcInsn(REST_HEADER_RESOURCE_NAME);
            mv.visitLdcInsn("");
            mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getHeader", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
            mv.visitMethodInsn(INVOKEINTERFACE, "java/util/Map", "get", "(Ljava/lang/Object;)Ljava/lang/Object;", true);
            mv.visitTypeInsn(CHECKCAST, serviceTypeInternalName);
            mv.visitLabel(lserif);
            mv.visitFrame(Opcodes.F_SAME1, 0, null, 1, new Object[]{serviceTypeInternalName});
            mv.visitVarInsn(ASTORE, 3);

            final int maxStack = 3 + params.length;
            List<int[]> varInsns = new ArrayList<>();
            int maxLocals = 4;

            List<String> asmParamNames = asmParamMap == null ? null : asmParamMap.get(method.getName() + ":" + Type.getMethodDescriptor(method));
            List<Object[]> paramlist = new ArrayList<>();
            //解析方法中的每个参数
            for (int i = 0; i < params.length; i++) {
                final Parameter param = params[i];
                final Class ptype = param.getType();
                String n = null;
                String comment = "";
                boolean required = true;
                int radix = 10;

                RestHeader annhead = param.getAnnotation(RestHeader.class);
                if (annhead != null) {
                    if (ptype != String.class) throw new RuntimeException("@RestHeader must on String Parameter in " + method);
                    n = annhead.name();
                    radix = annhead.radix();
                    comment = annhead.comment();
                    if (n.isEmpty()) throw new RuntimeException("@RestHeader.value is illegal in " + method);
                }
                RestCookie anncookie = param.getAnnotation(RestCookie.class);
                if (anncookie != null) {
                    if (annhead != null) throw new RuntimeException("@RestCookie and @RestHeader cannot on the same Parameter in " + method);
                    if (ptype != String.class) throw new RuntimeException("@RestCookie must on String Parameter in " + method);
                    n = anncookie.name();
                    radix = anncookie.radix();
                    comment = anncookie.comment();
                    if (n.isEmpty()) throw new RuntimeException("@RestCookie.value is illegal in " + method);
                }
                RestSessionid annsid = param.getAnnotation(RestSessionid.class);
                if (annsid != null) {
                    if (annhead != null) throw new RuntimeException("@RestSessionid and @RestHeader cannot on the same Parameter in " + method);
                    if (anncookie != null) throw new RuntimeException("@RestSessionid and @RestCookie cannot on the same Parameter in " + method);
                    if (ptype != String.class) throw new RuntimeException("@RestSessionid must on String Parameter in " + method);
                }
                RestAddress annaddr = param.getAnnotation(RestAddress.class);
                if (annaddr != null) {
                    if (annhead != null) throw new RuntimeException("@RestAddress and @RestHeader cannot on the same Parameter in " + method);
                    if (anncookie != null) throw new RuntimeException("@RestAddress and @RestCookie cannot on the same Parameter in " + method);
                    if (annsid != null) throw new RuntimeException("@RestAddress and @RestSessionid cannot on the same Parameter in " + method);
                    if (ptype != String.class) throw new RuntimeException("@RestAddress must on String Parameter in " + method);
                    comment = annaddr.comment();
                }
                RestBody annbody = param.getAnnotation(RestBody.class);
                if (annbody != null) {
                    if (annhead != null) throw new RuntimeException("@RestBody and @RestHeader cannot on the same Parameter in " + method);
                    if (anncookie != null) throw new RuntimeException("@RestBody and @RestCookie cannot on the same Parameter in " + method);
                    if (annsid != null) throw new RuntimeException("@RestBody and @RestSessionid cannot on the same Parameter in " + method);
                    if (annaddr != null) throw new RuntimeException("@RestBody and @RestAddress cannot on the same Parameter in " + method);
                    if (ptype.isPrimitive()) throw new RuntimeException("@RestBody cannot on primitive type Parameter in " + method);
                    comment = annbody.comment();
                }
                RestUploadFile annfile = param.getAnnotation(RestUploadFile.class);
                if (annfile != null) {
                    if (mupload != null) throw new RuntimeException("@RestUploadFile repeat in " + method);
                    mupload = annfile;
                    muploadType = ptype;
                    if (annhead != null) throw new RuntimeException("@RestUploadFile and @RestHeader cannot on the same Parameter in " + method);
                    if (anncookie != null) throw new RuntimeException("@RestUploadFile and @RestCookie cannot on the same Parameter in " + method);
                    if (annsid != null) throw new RuntimeException("@RestUploadFile and @RestSessionid cannot on the same Parameter in " + method);
                    if (annaddr != null) throw new RuntimeException("@RestUploadFile and @RestAddress cannot on the same Parameter in " + method);
                    if (annbody != null) throw new RuntimeException("@RestUploadFile and @RestBody cannot on the same Parameter in " + method);
                    if (ptype != byte[].class && ptype != File.class && ptype != File[].class) throw new RuntimeException("@RestUploadFile must on byte[] or File or File[] Parameter in " + method);
                    comment = annfile.comment();
                }

                RestURI annuri = param.getAnnotation(RestURI.class);
                if (annuri != null) {
                    if (annhead != null) throw new RuntimeException("@RestURI and @RestHeader cannot on the same Parameter in " + method);
                    if (anncookie != null) throw new RuntimeException("@RestURI and @RestCookie cannot on the same Parameter in " + method);
                    if (annsid != null) throw new RuntimeException("@RestURI and @RestSessionid cannot on the same Parameter in " + method);
                    if (annaddr != null) throw new RuntimeException("@RestURI and @RestAddress cannot on the same Parameter in " + method);
                    if (annbody != null) throw new RuntimeException("@RestURI and @RestBody cannot on the same Parameter in " + method);
                    if (annfile != null) throw new RuntimeException("@RestURI and @RestUploadFile cannot on the same Parameter in " + method);
                    if (ptype != String.class) throw new RuntimeException("@RestURI must on String Parameter in " + method);
                    comment = annuri.comment();
                }

                RestParam annpara = param.getAnnotation(RestParam.class);
                if (annpara != null) radix = annpara.radix();
                if (annpara != null) comment = annpara.comment();
                if (annpara != null) required = annpara.required();
                if (n == null) n = (annpara == null || annpara.name().isEmpty()) ? null : annpara.name();
                if (n == null && ptype == userType) n = "&"; //用户类型特殊处理
                if (n == null && asmParamNames != null && asmParamNames.size() > i) n = asmParamNames.get(i);
                if (n == null) {
                    if (param.isNamePresent()) {
                        n = param.getName();
                    } else if (ptype == Flipper.class) {
                        n = "flipper";
                    } else {
                        throw new RuntimeException("Parameter " + param.getName() + " not found name by @RestParam  in " + method);
                    }
                }
                if (annhead == null && anncookie == null && annaddr == null && annbody == null && annfile == null
                    && (entry.name.startsWith("find") || entry.name.startsWith("delete")) && params.length == 1) {
                    if (ptype.isPrimitive() || ptype == String.class) n = "#";
                }
                if (annhead == null && anncookie == null && annsid == null && annaddr == null && annbody == null && annfile == null
                    && !ptype.isPrimitive() && ptype != String.class && ptype != Flipper.class && !CompletionHandler.class.isAssignableFrom(ptype)
                    && !ptype.getName().startsWith("java") && n.charAt(0) != '#' && !"&".equals(n)) { //判断Json对象是否包含@RestUploadFile
                    Class loop = ptype;
                    do {
                        if (loop == null || loop.isInterface()) break; //接口时getSuperclass可能会得到null
                        for (Field field : loop.getDeclaredFields()) {
                            if (Modifier.isStatic(field.getModifiers())) continue;
                            if (Modifier.isFinal(field.getModifiers())) continue;
                            RestUploadFile ruf = field.getAnnotation(RestUploadFile.class);
                            if (ruf == null) continue;
                            if (mupload != null) throw new RuntimeException("@RestUploadFile repeat in " + method + " or field " + field);
                            mupload = ruf;
                            muploadType = field.getType();
                        }
                    } while ((loop = loop.getSuperclass()) != Object.class);
                }
                paramlist.add(new Object[]{param, n, ptype, radix, comment, required, annpara, annsid, annaddr, annhead, anncookie, annbody, annfile, annuri, param.getParameterizedType()});
            }

            Map<String, Object> mappingMap = new LinkedHashMap<>();
            { // 设置 Annotation
                //设置 HttpMapping
                boolean reqpath = false;
                for (Object[] ps : paramlist) {
                    if ("#".equals((String) ps[1])) {
                        reqpath = true;
                        break;
                    }
                }
                av0 = mv.visitAnnotation(mappingDesc, true);
                String url = (catalog.isEmpty() ? "/" : ("/" + catalog + "/")) + (defmodulename.isEmpty() ? "" : (defmodulename + "/")) + entry.name + (reqpath ? "/" : "");
                av0.visit("url", url);
                av0.visit("auth", entry.auth);
                av0.visit("cacheseconds", entry.cacheseconds);
                av0.visit("actionid", entry.actionid);
                av0.visit("comment", entry.comment);

                AnnotationVisitor av1 = av0.visitArray("methods");
                for (String m : entry.methods) {
                    av1.visit(null, m);
                }
                av1.visitEnd();

                java.lang.reflect.Type grt = TypeToken.getGenericType(method.getGenericReturnType(), serviceType);
                av0.visit("result", grt == returnType ? returnType.getName() : String.valueOf(grt));

                av0.visitEnd();
                mappingMap.put("url", url);
                mappingMap.put("auth", entry.auth);
                mappingMap.put("cacheseconds", entry.cacheseconds);
                mappingMap.put("actionid", entry.actionid);
                mappingMap.put("comment", entry.comment);
                mappingMap.put("methods", entry.methods);
                mappingMap.put("result", grt == returnType ? returnType.getName() : String.valueOf(grt));
            }

            { // 设置 Annotation
                av0 = mv.visitAnnotation(webparamsDesc, true);
                AnnotationVisitor av1 = av0.visitArray("value");
                //设置 WebParam
                for (Object[] ps : paramlist) { //{param, n, ptype, radix, comment, required, annpara, annsid, annaddr, annhead, anncookie, annbody, annfile, annuri, pgentype}   
                    final boolean ishead = ((RestHeader) ps[9]) != null; //是否取getHeader 而不是 getParameter
                    final boolean iscookie = ((RestCookie) ps[10]) != null; //是否取getCookie

                    AnnotationVisitor av2 = av1.visitAnnotation(null, webparamDesc);
                    av2.visit("name", (String) ps[1]);
                    av2.visit("type", Type.getType(Type.getDescriptor((Class) ps[2])));
                    av2.visit("radix", (Integer) ps[3]);
                    av2.visitEnum("src", sourcetypeDesc, ishead ? HttpParam.HttpParamSourceType.HEADER.name()
                        : (iscookie ? HttpParam.HttpParamSourceType.COOKIE.name() : HttpParam.HttpParamSourceType.PARAMETER.name()));
                    av2.visit("comment", (String) ps[4]);
                    av2.visit("required", (Boolean) ps[5]);
                    av2.visitEnd();
                }
                av1.visitEnd();
                av0.visitEnd();
            }
            int uploadLocal = 0;
            if (mupload != null) { //存在文件上传
                if (muploadType == byte[].class) {
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getMultiContext", "()" + multiContextDesc, false);
                    mv.visitLdcInsn(mupload.maxLength());
                    mv.visitLdcInsn(mupload.fileNameReg());
                    mv.visitLdcInsn(mupload.contentTypeReg());
                    mv.visitMethodInsn(INVOKEVIRTUAL, multiContextName, "partsFirstBytes", "(JLjava/lang/String;Ljava/lang/String;)[B", false);
                    mv.visitVarInsn(ASTORE, maxLocals);
                    uploadLocal = maxLocals;
                } else if (muploadType == File.class) {
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getMultiContext", "()" + multiContextDesc, false);
                    mv.visitVarInsn(ALOAD, 0);
                    mv.visitFieldInsn(GETFIELD, newDynName, "_redkale_home", "Ljava/io/File;");
                    mv.visitLdcInsn(mupload.maxLength());
                    mv.visitLdcInsn(mupload.fileNameReg());
                    mv.visitLdcInsn(mupload.contentTypeReg());
                    mv.visitMethodInsn(INVOKEVIRTUAL, multiContextName, "partsFirstFile", "(Ljava/io/File;JLjava/lang/String;Ljava/lang/String;)Ljava/io/File;", false);
                    mv.visitVarInsn(ASTORE, maxLocals);
                    uploadLocal = maxLocals;
                } else if (muploadType == File[].class) { //File[]
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getMultiContext", "()" + multiContextDesc, false);
                    mv.visitVarInsn(ALOAD, 0);
                    mv.visitFieldInsn(GETFIELD, newDynName, "_redkale_home", "Ljava/io/File;");
                    mv.visitLdcInsn(mupload.maxLength());
                    mv.visitLdcInsn(mupload.fileNameReg());
                    mv.visitLdcInsn(mupload.contentTypeReg());
                    mv.visitMethodInsn(INVOKEVIRTUAL, multiContextName, "partsFiles", "(Ljava/io/File;JLjava/lang/String;Ljava/lang/String;)[Ljava/io/File;", false);
                    mv.visitVarInsn(ASTORE, maxLocals);
                    uploadLocal = maxLocals;
                }
                maxLocals++;
            }

            List<Map<String, Object>> paramMaps = new ArrayList<>();
            //获取每个参数的值
            boolean hasAsyncHandler = false;
            for (Object[] ps : paramlist) {
                Map<String, Object> paramMap = new LinkedHashMap<>();
                final Parameter param = (Parameter) ps[0]; //参数类型
                String pname = (String) ps[1]; //参数名
                Class ptype = (Class) ps[2]; //参数类型
                int radix = (Integer) ps[3];
                String comment = (String) ps[4];
                boolean required = (Boolean) ps[5];
                RestParam annpara = (RestParam) ps[6];
                RestSessionid annsid = (RestSessionid) ps[7];
                RestAddress annaddr = (RestAddress) ps[8];
                RestHeader annhead = (RestHeader) ps[9];
                RestCookie anncookie = (RestCookie) ps[10];
                RestBody annbody = (RestBody) ps[11];
                RestUploadFile annfile = (RestUploadFile) ps[12];
                RestURI annuri = (RestURI) ps[13];
                java.lang.reflect.Type pgentype = (java.lang.reflect.Type) ps[14];

                final boolean ishead = annhead != null; //是否取getHeader 而不是 getParameter
                final boolean iscookie = anncookie != null; //是否取getCookie

                paramMap.put("name", pname);
                paramMap.put("type", ptype.getName());
                if (CompletionHandler.class.isAssignableFrom(ptype)) { //HttpResponse.createAsyncHandler() or HttpResponse.createAsyncHandler(Class)
                    if (ptype == CompletionHandler.class) {
                        mv.visitVarInsn(ALOAD, 2);
                        mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "createAsyncHandler", "()Ljava/nio/channels/CompletionHandler;", false);
                        mv.visitVarInsn(ASTORE, maxLocals);
                        varInsns.add(new int[]{ALOAD, maxLocals});
                    } else {
                        mv.visitVarInsn(ALOAD, 3);
                        mv.visitVarInsn(ALOAD, 2);
                        mv.visitLdcInsn(Type.getType(Type.getDescriptor(ptype)));
                        mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "createAsyncHandler", "(Ljava/lang/Class;)Ljava/nio/channels/CompletionHandler;", false);
                        mv.visitTypeInsn(CHECKCAST, ptype.getName().replace('.', '/'));
                        mv.visitVarInsn(ASTORE, maxLocals);
                        varInsns.add(new int[]{ALOAD, maxLocals});
                    }
                    hasAsyncHandler = true;
                } else if (annsid != null) { //HttpRequest.getSessionid(true|false)
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitInsn(annsid.create() ? ICONST_1 : ICONST_0);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getSessionid", "(Z)Ljava/lang/String;", false);
                    mv.visitVarInsn(ASTORE, maxLocals);
                    varInsns.add(new int[]{ALOAD, maxLocals});
                } else if (annaddr != null) { //HttpRequest.getRemoteAddr
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRemoteAddr", "()Ljava/lang/String;", false);
                    mv.visitVarInsn(ASTORE, maxLocals);
                    varInsns.add(new int[]{ALOAD, maxLocals});
                } else if (annbody != null) { //HttpRequest.getBodyUTF8 / HttpRequest.getBody
                    if (ptype == String.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getBodyUTF8", "()Ljava/lang/String;", false);
                        mv.visitVarInsn(ASTORE, maxLocals);
                        varInsns.add(new int[]{ALOAD, maxLocals});
                    } else if (ptype == byte[].class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getBody", "()[B", false);
                        mv.visitVarInsn(ASTORE, maxLocals);
                        varInsns.add(new int[]{ALOAD, maxLocals});
                    } else { //JavaBean 转 Json
                        String typefieldname = "_redkale_body_jsontype_" + bodyTypes.size();
                        bodyTypes.put(typefieldname, pgentype);
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitVarInsn(ALOAD, 0);
                        mv.visitFieldInsn(GETFIELD, newDynName, typefieldname, "Ljava/lang/reflect/Type;");
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getBodyJson", "(Ljava/lang/reflect/Type;)Ljava/lang/Object;", false);
                        mv.visitTypeInsn(CHECKCAST, Type.getInternalName(ptype));
                        mv.visitVarInsn(ASTORE, maxLocals);
                        varInsns.add(new int[]{ALOAD, maxLocals});
                    }
                } else if (annfile != null) { //MultiContext.partsFirstBytes / HttpRequest.partsFirstFile / HttpRequest.partsFiles
                    mv.visitVarInsn(ALOAD, 4);
                    mv.visitVarInsn(ASTORE, maxLocals);
                    varInsns.add(new int[]{ALOAD, maxLocals});
                } else if (annuri != null) { //HttpRequest.getRequestURI
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequestURI", "()Ljava/lang/String;", false);
                    mv.visitVarInsn(ASTORE, maxLocals);
                    varInsns.add(new int[]{ALOAD, maxLocals});
                } else if ("#".equals(pname)) { //从request.getRequstURI 中取参数
                    if (ptype == boolean.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURILastPath", "()Ljava/lang/String;", false);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "parseBoolean", "(Ljava/lang/String;)Z", false);
                        mv.visitVarInsn(ISTORE, maxLocals);
                        varInsns.add(new int[]{ILOAD, maxLocals});
                    } else if (ptype == byte.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURILastPath", "()Ljava/lang/String;", false);
                        mv.visitIntInsn(BIPUSH, radix);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Byte", "parseByte", "(Ljava/lang/String;I)B", false);
                        mv.visitVarInsn(ISTORE, maxLocals);
                        varInsns.add(new int[]{ILOAD, maxLocals});
                    } else if (ptype == short.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURILastPath", "()Ljava/lang/String;", false);
                        mv.visitIntInsn(BIPUSH, radix);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Short", "parseShort", "(Ljava/lang/String;I)S", false);
                        mv.visitVarInsn(ISTORE, maxLocals);
                        varInsns.add(new int[]{ILOAD, maxLocals});
                    } else if (ptype == char.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURILastPath", "()Ljava/lang/String;", false);
                        mv.visitInsn(ICONST_0);
                        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "charAt", "(I)C", false);
                        mv.visitVarInsn(ISTORE, maxLocals);
                        varInsns.add(new int[]{ILOAD, maxLocals});
                    } else if (ptype == int.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURILastPath", "()Ljava/lang/String;", false);
                        mv.visitIntInsn(BIPUSH, radix);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "parseInt", "(Ljava/lang/String;I)I", false);
                        mv.visitVarInsn(ISTORE, maxLocals);
                        varInsns.add(new int[]{ILOAD, maxLocals});
                    } else if (ptype == float.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURILastPath", "()Ljava/lang/String;", false);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Float", "parseFloat", "(Ljava/lang/String;)F", false);
                        mv.visitVarInsn(FSTORE, maxLocals);
                        varInsns.add(new int[]{FLOAD, maxLocals});
                    } else if (ptype == long.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURILastPath", "()Ljava/lang/String;", false);
                        mv.visitIntInsn(BIPUSH, radix);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Long", "parseLong", "(Ljava/lang/String;I)J", false);
                        mv.visitVarInsn(LSTORE, maxLocals);
                        varInsns.add(new int[]{LLOAD, maxLocals});
                        maxLocals++;
                    } else if (ptype == double.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURILastPath", "()Ljava/lang/String;", false);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Double", "parseDouble", "(Ljava/lang/String;)D", false);
                        mv.visitVarInsn(DSTORE, maxLocals);
                        varInsns.add(new int[]{DLOAD, maxLocals});
                        maxLocals++;
                    } else if (ptype == String.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURILastPath", "()Ljava/lang/String;", false);
                        mv.visitVarInsn(ASTORE, maxLocals);
                        varInsns.add(new int[]{ALOAD, maxLocals});
                    } else {
                        throw new RuntimeException(method + " only " + RestParam.class.getSimpleName() + "(#) to Type(primitive class or String)");
                    }
                } else if (pname.charAt(0) == '#') { //从request.getRequstURIPath 中去参数
                    if (ptype == boolean.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitLdcInsn(pname.substring(1));
                        mv.visitLdcInsn("false");
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURIPath", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "parseBoolean", "(Ljava/lang/String;)Z", false);
                        mv.visitVarInsn(ISTORE, maxLocals);
                        varInsns.add(new int[]{ILOAD, maxLocals});
                    } else if (ptype == byte.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitLdcInsn(pname.substring(1));
                        mv.visitLdcInsn("0");
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURIPath", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                        mv.visitIntInsn(BIPUSH, radix);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Byte", "parseByte", "(Ljava/lang/String;I)B", false);
                        mv.visitVarInsn(ISTORE, maxLocals);
                        varInsns.add(new int[]{ILOAD, maxLocals});
                    } else if (ptype == short.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitLdcInsn(pname.substring(1));
                        mv.visitLdcInsn("0");
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURIPath", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                        mv.visitIntInsn(BIPUSH, radix);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Short", "parseShort", "(Ljava/lang/String;I)S", false);
                        mv.visitVarInsn(ISTORE, maxLocals);
                        varInsns.add(new int[]{ILOAD, maxLocals});
                    } else if (ptype == char.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitLdcInsn(pname.substring(1));
                        mv.visitLdcInsn("0");
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURIPath", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                        mv.visitInsn(ICONST_0);
                        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "charAt", "(I)C", false);
                        mv.visitVarInsn(ISTORE, maxLocals);
                        varInsns.add(new int[]{ILOAD, maxLocals});
                    } else if (ptype == int.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitLdcInsn(pname.substring(1));
                        mv.visitLdcInsn("0");
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURIPath", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                        mv.visitIntInsn(BIPUSH, radix);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "parseInt", "(Ljava/lang/String;I)I", false);
                        mv.visitVarInsn(ISTORE, maxLocals);
                        varInsns.add(new int[]{ILOAD, maxLocals});
                    } else if (ptype == float.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitLdcInsn(pname.substring(1));
                        mv.visitLdcInsn("0");
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURIPath", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Float", "parseFloat", "(Ljava/lang/String;)F", false);
                        mv.visitVarInsn(FSTORE, maxLocals);
                        varInsns.add(new int[]{FLOAD, maxLocals});
                    } else if (ptype == long.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitLdcInsn(pname.substring(1));
                        mv.visitLdcInsn("0");
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURIPath", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                        mv.visitIntInsn(BIPUSH, radix);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Long", "parseLong", "(Ljava/lang/String;I)J", false);
                        mv.visitVarInsn(LSTORE, maxLocals);
                        varInsns.add(new int[]{LLOAD, maxLocals});
                        maxLocals++;
                    } else if (ptype == double.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitLdcInsn(pname.substring(1));
                        mv.visitLdcInsn("0");
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURIPath", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Double", "parseDouble", "(Ljava/lang/String;)D", false);
                        mv.visitVarInsn(DSTORE, maxLocals);
                        varInsns.add(new int[]{DLOAD, maxLocals});
                        maxLocals++;
                    } else if (ptype == String.class) {
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitLdcInsn(pname.substring(1));
                        mv.visitLdcInsn("");
                        mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequstURIPath", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                        mv.visitVarInsn(ASTORE, maxLocals);
                        varInsns.add(new int[]{ALOAD, maxLocals});
                    } else {
                        throw new RuntimeException(method + " only " + RestParam.class.getSimpleName() + "(#) to Type(primitive class or String)");
                    }
                } else if ("&".equals(pname) && ptype == userType) { //当前用户对象的类名
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "currentUser", "()Ljava/lang/Object;", false);
                    mv.visitTypeInsn(CHECKCAST, Type.getInternalName(ptype));
                    mv.visitVarInsn(ASTORE, maxLocals);
                    varInsns.add(new int[]{ALOAD, maxLocals});
                } else if (ptype == boolean.class) {
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitLdcInsn(pname);
                    mv.visitInsn(ICONST_0);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, ishead ? "getBooleanHeader" : "getBooleanParameter", "(Ljava/lang/String;Z)Z", false);
                    mv.visitVarInsn(ISTORE, maxLocals);
                    varInsns.add(new int[]{ILOAD, maxLocals});
                } else if (ptype == byte.class) {
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitLdcInsn(pname);
                    mv.visitLdcInsn("0");
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, ishead ? "getHeader" : "getParameter", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                    mv.visitIntInsn(BIPUSH, radix);
                    mv.visitMethodInsn(INVOKESTATIC, "java/lang/Byte", "parseByte", "(Ljava/lang/String;I)B", false);
                    mv.visitVarInsn(ISTORE, maxLocals);
                    varInsns.add(new int[]{ILOAD, maxLocals});
                } else if (ptype == short.class) {
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitIntInsn(BIPUSH, radix);
                    mv.visitLdcInsn(pname);
                    mv.visitInsn(ICONST_0);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, ishead ? "getShortHeader" : "getShortParameter", "(ILjava/lang/String;S)S", false);
                    mv.visitVarInsn(ISTORE, maxLocals);
                    varInsns.add(new int[]{ILOAD, maxLocals});
                } else if (ptype == char.class) {
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitLdcInsn(pname);
                    mv.visitLdcInsn("0");
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, ishead ? "getHeader" : "getParameter", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                    mv.visitInsn(ICONST_0);
                    mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "charAt", "(I)C", false);
                    mv.visitVarInsn(ISTORE, maxLocals);
                    varInsns.add(new int[]{ILOAD, maxLocals});
                } else if (ptype == int.class) {
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitIntInsn(BIPUSH, radix);
                    mv.visitLdcInsn(pname);
                    mv.visitInsn(ICONST_0);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, ishead ? "getIntHeader" : "getIntParameter", "(ILjava/lang/String;I)I", false);
                    mv.visitVarInsn(ISTORE, maxLocals);
                    varInsns.add(new int[]{ILOAD, maxLocals});
                } else if (ptype == float.class) {
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitLdcInsn(pname);
                    mv.visitInsn(FCONST_0);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, ishead ? "getFloatHeader" : "getFloatParameter", "(Ljava/lang/String;F)F", false);
                    mv.visitVarInsn(FSTORE, maxLocals);
                    varInsns.add(new int[]{FLOAD, maxLocals});
                } else if (ptype == long.class) {
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitIntInsn(BIPUSH, radix);
                    mv.visitLdcInsn(pname);
                    mv.visitInsn(LCONST_0);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, ishead ? "getLongHeader" : "getLongParameter", "(ILjava/lang/String;J)J", false);
                    mv.visitVarInsn(LSTORE, maxLocals);
                    varInsns.add(new int[]{LLOAD, maxLocals});
                    maxLocals++;
                } else if (ptype == double.class) {
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitLdcInsn(pname);
                    mv.visitInsn(DCONST_0);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, ishead ? "getDoubleHeader" : "getDoubleParameter", "(Ljava/lang/String;D)D", false);
                    mv.visitVarInsn(DSTORE, maxLocals);
                    varInsns.add(new int[]{DLOAD, maxLocals});
                    maxLocals++;
                } else if (ptype == String.class) {
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitLdcInsn(pname);
                    mv.visitLdcInsn("");
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, iscookie ? "getCookie" : (ishead ? "getHeader" : "getParameter"), "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                    mv.visitVarInsn(ASTORE, maxLocals);
                    varInsns.add(new int[]{ALOAD, maxLocals});
                } else if (ptype == Flipper.class) {
                    mv.visitVarInsn(ALOAD, 1);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getFlipper", "()" + flipperDesc, false);
                    mv.visitVarInsn(ASTORE, maxLocals);
                    varInsns.add(new int[]{ALOAD, maxLocals});
                } else { //其他Json对象
                    mv.visitVarInsn(ALOAD, 1);
                    if (param.getType() == param.getParameterizedType()) {
                        mv.visitLdcInsn(Type.getType(Type.getDescriptor(ptype)));
                    } else {
                        mv.visitVarInsn(ALOAD, 0);
                        mv.visitFieldInsn(GETFIELD, newDynName, REST_PARAMTYPES_FIELD_NAME, "[[Ljava/lang/reflect/Type;");
                        pushInt(mv, entry.methodidx);//方法下标
                        mv.visitInsn(AALOAD);
                        int paramidx = 0;
                        for (int i = 0; i < params.length; i++) {
                            if (params[i] == param) {
                                paramidx = i;
                                break;
                            }
                        }
                        pushInt(mv, paramidx); //参数下标
                        mv.visitInsn(AALOAD);
                    }
                    mv.visitLdcInsn(pname);
                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, ishead ? "getJsonHeader" : "getJsonParameter", "(Ljava/lang/reflect/Type;Ljava/lang/String;)Ljava/lang/Object;", false);
                    mv.visitTypeInsn(CHECKCAST, ptype.getName().replace('.', '/'));
                    mv.visitVarInsn(ASTORE, maxLocals);
                    varInsns.add(new int[]{ALOAD, maxLocals});

                    //构建 RestHeader、RestCookie、RestAddress 等赋值操作
                    Class loop = ptype;
                    Set<String> fields = new HashSet<>();
                    Map<String, Object[]> attrParaNames = new LinkedHashMap<>();
                    do {
                        if (loop == null || loop.isInterface()) break; //接口时getSuperclass可能会得到null
                        for (Field field : loop.getDeclaredFields()) {
                            if (Modifier.isStatic(field.getModifiers())) continue;
                            if (Modifier.isFinal(field.getModifiers())) continue;
                            if (fields.contains(field.getName())) continue;
                            RestHeader rh = field.getAnnotation(RestHeader.class);
                            RestCookie rc = field.getAnnotation(RestCookie.class);
                            RestSessionid rs = field.getAnnotation(RestSessionid.class);
                            RestAddress ra = field.getAnnotation(RestAddress.class);
                            RestBody rb = field.getAnnotation(RestBody.class);
                            RestUploadFile ru = field.getAnnotation(RestUploadFile.class);
                            RestURI ri = field.getAnnotation(RestURI.class);
                            if (rh == null && rc == null && ra == null && rb == null && rs == null && ru == null && ri == null) continue;
                            if (rh != null && field.getType() != String.class) throw new RuntimeException("@RestHeader must on String Field in " + field);
                            if (rc != null && field.getType() != String.class) throw new RuntimeException("@RestCookie must on String Field in " + field);
                            if (rs != null && field.getType() != String.class) throw new RuntimeException("@RestSessionid must on String Field in " + field);
                            if (ra != null && field.getType() != String.class) throw new RuntimeException("@RestAddress must on String Field in " + field);
                            if (rb != null && field.getType().isPrimitive()) throw new RuntimeException("@RestBody must on cannot on primitive type Field in " + field);
                            if (ru != null && field.getType() != byte[].class && field.getType() != File.class && field.getType() != File[].class) {
                                throw new RuntimeException("@RestUploadFile must on byte[] or File or File[] Field in " + field);
                            }

                            if (ri != null && field.getType() != String.class) throw new RuntimeException("@RestURI must on String Field in " + field);
                            org.redkale.util.Attribute attr = org.redkale.util.Attribute.create(loop, field);
                            String attrFieldName;
                            String restname = "";
                            if (rh != null) {
                                attrFieldName = "_redkale_attr_header_" + restAttributes.size();
                                restname = rh.name();
                            } else if (rc != null) {
                                attrFieldName = "_redkale_attr_cookie_" + restAttributes.size();
                                restname = rc.name();
                            } else if (rs != null) {
                                attrFieldName = "_redkale_attr_sessionid_" + restAttributes.size();
                                restname = rs.create() ? "1" : ""; //用于下面区分create值
                            } else if (ra != null) {
                                attrFieldName = "_redkale_attr_address_" + restAttributes.size();
                                //restname = "";
                            } else if (rb != null && field.getType() == String.class) {
                                attrFieldName = "_redkale_attr_bodystring_" + restAttributes.size();
                                //restname = "";
                            } else if (rb != null && field.getType() == byte[].class) {
                                attrFieldName = "_redkale_attr_bodybytes_" + restAttributes.size();
                                //restname = "";
                            } else if (rb != null && field.getType() != String.class && field.getType() != byte[].class) {
                                attrFieldName = "_redkale_attr_bodyjson_" + restAttributes.size();
                                //restname = "";
                            } else if (ru != null && field.getType() == byte[].class) {
                                attrFieldName = "_redkale_attr_uploadbytes_" + restAttributes.size();
                                //restname = "";
                            } else if (ru != null && field.getType() == File.class) {
                                attrFieldName = "_redkale_attr_uploadfile_" + restAttributes.size();
                                //restname = "";
                            } else if (ru != null && field.getType() == File[].class) {
                                attrFieldName = "_redkale_attr_uploadfiles_" + restAttributes.size();
                                //restname = "";
                            } else if (ri != null && field.getType() == String.class) {
                                attrFieldName = "_redkale_attr_uri_" + restAttributes.size();
                                //restname = "";
                            } else {
                                continue;
                            }
                            restAttributes.put(attrFieldName, attr);
                            attrParaNames.put(attrFieldName, new Object[]{restname, field.getType(), field.getGenericType(), ru});
                            fields.add(field.getName());
                        }
                    } while ((loop = loop.getSuperclass()) != Object.class);

                    if (!attrParaNames.isEmpty()) { //参数存在 RestHeader、RestCookie、RestSessionid、RestAddress、RestBody字段
                        mv.visitVarInsn(ALOAD, maxLocals); //加载JsonBean
                        Label lif = new Label();
                        mv.visitJumpInsn(IFNULL, lif);  //if(bean != null) {
                        for (Map.Entry<String, Object[]> en : attrParaNames.entrySet()) {
                            RestUploadFile ru = (RestUploadFile) en.getValue()[3];
                            mv.visitVarInsn(ALOAD, 0);
                            mv.visitFieldInsn(GETFIELD, newDynName, en.getKey(), attrDesc);
                            mv.visitVarInsn(ALOAD, maxLocals);
                            mv.visitVarInsn(ALOAD, en.getKey().contains("_upload") ? uploadLocal : 1);
                            if (en.getKey().contains("_header_")) {
                                String headerkey = en.getValue()[0].toString();
                                if ("Host".equalsIgnoreCase(headerkey)) {
                                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getHost", "()Ljava/lang/String;", false);
                                } else if ("Content-Type".equalsIgnoreCase(headerkey)) {
                                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getContentType", "()Ljava/lang/String;", false);
                                } else if ("Connection".equalsIgnoreCase(headerkey)) {
                                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getConnection", "()Ljava/lang/String;", false);
                                } else if ("Method".equalsIgnoreCase(headerkey)) {
                                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getMethod", "()Ljava/lang/String;", false);
                                } else {
                                    mv.visitLdcInsn(headerkey);
                                    mv.visitLdcInsn("");
                                    mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getHeader", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                                }
                            } else if (en.getKey().contains("_cookie_")) {
                                mv.visitLdcInsn(en.getValue()[0].toString());
                                mv.visitLdcInsn("");
                                mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getCookie", "(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;", false);
                            } else if (en.getKey().contains("_sessionid_")) {
                                mv.visitInsn(en.getValue()[0].toString().isEmpty() ? ICONST_0 : ICONST_1);
                                mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getSessionid", "(Z)Ljava/lang/String;", false);
                            } else if (en.getKey().contains("_address_")) {
                                mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRemoteAddr", "()Ljava/lang/String;", false);
                            } else if (en.getKey().contains("_uri_")) {
                                mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getRequestURI", "()Ljava/lang/String;", false);
                            } else if (en.getKey().contains("_bodystring_")) {
                                mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getBodyUTF8", "()Ljava/lang/String;", false);
                            } else if (en.getKey().contains("_bodybytes_")) {
                                mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getBody", "()[B", false);
                            } else if (en.getKey().contains("_bodyjson_")) {//JavaBean 转 Json
                                String typefieldname = "_redkale_body_jsontype_" + bodyTypes.size();
                                bodyTypes.put(typefieldname, (java.lang.reflect.Type) en.getValue()[2]);
                                mv.visitVarInsn(ALOAD, 0);
                                mv.visitFieldInsn(GETFIELD, newDynName, typefieldname, "Ljava/lang/reflect/Type;");
                                mv.visitMethodInsn(INVOKEVIRTUAL, reqInternalName, "getBodyJson", "(Ljava/lang/reflect/Type;)Ljava/lang/Object;", false);
                                mv.visitTypeInsn(CHECKCAST, Type.getInternalName((Class) en.getValue()[1]));
                            } else if (en.getKey().contains("_uploadbytes_")) {
                                //只需mv.visitVarInsn(ALOAD, 4), 无需处理
                            } else if (en.getKey().contains("_uploadfile_")) {
                                //只需mv.visitVarInsn(ALOAD, 4), 无需处理                                
                            } else if (en.getKey().contains("_uploadfiles_")) {
                                //只需mv.visitVarInsn(ALOAD, 4), 无需处理                                
                            }
                            mv.visitMethodInsn(INVOKEINTERFACE, attrInternalName, "set", "(Ljava/lang/Object;Ljava/lang/Object;)V", true);
                        }
                        mv.visitLabel(lif); // end if }
                        mv.visitFrame(Opcodes.F_APPEND, 1, new Object[]{ptype.getName().replace('.', '/')}, 0, null);
                    }
                }
                maxLocals++;
                paramMaps.add(paramMap);
            } // end params for each

            //mv.visitVarInsn(ALOAD, 0); //调用this
            //mv.visitFieldInsn(GETFIELD, newDynName, REST_SERVICE_FIELD_NAME, serviceDesc);
            mv.visitVarInsn(ALOAD, 3);
            for (int[] ins : varInsns) {
                mv.visitVarInsn(ins[0], ins[1]);
            }
            mv.visitMethodInsn(INVOKEVIRTUAL, serviceTypeInternalName, method.getName(), methodDesc, false);
            if (hasAsyncHandler) {
                mv.visitInsn(RETURN);
            } else if (returnType == void.class) {
                mv.visitVarInsn(ALOAD, 2);
                mv.visitMethodInsn(INVOKESTATIC, retInternalName, "success", "()" + retDesc, false);
                mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finishJson", "(" + retDesc + ")V", false);
                mv.visitInsn(RETURN);
            } else if (returnType == boolean.class) {
                mv.visitVarInsn(ISTORE, maxLocals);
                mv.visitVarInsn(ALOAD, 2); //response
                mv.visitVarInsn(ILOAD, maxLocals);
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/String", "valueOf", "(Z)Ljava/lang/String;", false);
                mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finish", "(Ljava/lang/String;)V", false);
                mv.visitInsn(RETURN);
                maxLocals++;
            } else if (returnType == byte.class) {
                mv.visitVarInsn(ISTORE, maxLocals);
                mv.visitVarInsn(ALOAD, 2); //response
                mv.visitVarInsn(ILOAD, maxLocals);
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/String", "valueOf", "(I)Ljava/lang/String;", false);
                mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finish", "(Ljava/lang/String;)V", false);
                mv.visitInsn(RETURN);
                maxLocals++;
            } else if (returnType == short.class) {
                mv.visitVarInsn(ISTORE, maxLocals);
                mv.visitVarInsn(ALOAD, 2); //response
                mv.visitVarInsn(ILOAD, maxLocals);
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/String", "valueOf", "(I)Ljava/lang/String;", false);
                mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finish", "(Ljava/lang/String;)V", false);
                mv.visitInsn(RETURN);
                maxLocals++;
            } else if (returnType == char.class) {
                mv.visitVarInsn(ISTORE, maxLocals);
                mv.visitVarInsn(ALOAD, 2); //response
                mv.visitVarInsn(ILOAD, maxLocals);
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/String", "valueOf", "(C)Ljava/lang/String;", false);
                mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finish", "(Ljava/lang/String;)V", false);
                mv.visitInsn(RETURN);
                maxLocals++;
            } else if (returnType == int.class) {
                mv.visitVarInsn(ISTORE, maxLocals);
                mv.visitVarInsn(ALOAD, 2); //response
                mv.visitVarInsn(ILOAD, maxLocals);
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/String", "valueOf", "(I)Ljava/lang/String;", false);
                mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finish", "(Ljava/lang/String;)V", false);
                mv.visitInsn(RETURN);
                maxLocals++;
            } else if (returnType == float.class) {
                mv.visitVarInsn(FSTORE, maxLocals);
                mv.visitVarInsn(ALOAD, 2); //response
                mv.visitVarInsn(FLOAD, maxLocals);
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/String", "valueOf", "(F)Ljava/lang/String;", false);
                mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finish", "(Ljava/lang/String;)V", false);
                mv.visitInsn(RETURN);
                maxLocals++;
            } else if (returnType == long.class) {
                mv.visitVarInsn(LSTORE, maxLocals);
                mv.visitVarInsn(ALOAD, 2); //response
                mv.visitVarInsn(LLOAD, maxLocals);
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/String", "valueOf", "(J)Ljava/lang/String;", false);
                mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finish", "(Ljava/lang/String;)V", false);
                mv.visitInsn(RETURN);
                maxLocals += 2;
            } else if (returnType == double.class) {
                mv.visitVarInsn(DSTORE, maxLocals);
                mv.visitVarInsn(ALOAD, 2); //response
                mv.visitVarInsn(DLOAD, maxLocals);
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/String", "valueOf", "(D)Ljava/lang/String;", false);
                mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finish", "(Ljava/lang/String;)V", false);
                mv.visitInsn(RETURN);
                maxLocals += 2;
            } else if (returnType == String.class) {
                mv.visitVarInsn(ASTORE, maxLocals);
                mv.visitVarInsn(ALOAD, 2); //response
                mv.visitVarInsn(ALOAD, maxLocals);
                mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finish", "(Ljava/lang/String;)V", false);
                mv.visitInsn(RETURN);
                maxLocals++;
            } else if (returnType == File.class) {
                mv.visitVarInsn(ASTORE, maxLocals);
                mv.visitVarInsn(ALOAD, 2); //response
                mv.visitVarInsn(ALOAD, maxLocals);
                mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finish", "(Ljava/io/File;)V", false);
                mv.visitInsn(RETURN);
                maxLocals++;
            } else if (Number.class.isAssignableFrom(returnType) || CharSequence.class.isAssignableFrom(returnType)) {   //returnType == String.class 必须放在前面
                mv.visitVarInsn(ASTORE, maxLocals);
                mv.visitVarInsn(ALOAD, 2); //response
                mv.visitVarInsn(ALOAD, maxLocals);
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/String", "valueOf", "(Ljava/lang/Object;)Ljava/lang/String;", false);
                mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finish", "(Ljava/lang/String;)V", false);
                mv.visitInsn(RETURN);
                maxLocals++;
            } else {
                mv.visitVarInsn(ASTORE, maxLocals);
                mv.visitVarInsn(ALOAD, 2); //response
                if (rcs != null && rcs.length > 0) {
                    mv.visitVarInsn(ALOAD, 0);
                    mv.visitFieldInsn(GETFIELD, newDynName, REST_JSONCONVERT_FIELD_PREFIX + restConverts.size(), jsonConvertDesc);
                    mv.visitVarInsn(ALOAD, maxLocals);
                    mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finish", "(" + convertDesc + "Ljava/lang/Object;)V", false);
                } else {
                    mv.visitVarInsn(ALOAD, maxLocals);
                    mv.visitMethodInsn(INVOKEVIRTUAL, respInternalName, "finish", "(Ljava/lang/Object;)V", false);
                }
                mv.visitInsn(RETURN);
                maxLocals++;
            }
            mv.visitMaxs(maxStack, maxLocals);
            mappingMap.put("params", paramMaps);
            mappingMaps.add(mappingMap);
        } // end  for each 

        for (Map.Entry<String, java.lang.reflect.Type> en : bodyTypes.entrySet()) {
            fv = cw.visitField(ACC_PRIVATE, en.getKey(), "Ljava/lang/reflect/Type;", null, null);
            fv.visitEnd();
        }

        for (String attrname : restAttributes.keySet()) {
            fv = cw.visitField(ACC_PRIVATE, attrname, attrDesc, null, null);
            fv.visitEnd();
        }

        for (int i = 1; i <= restConverts.size(); i++) {
            fv = cw.visitField(ACC_PRIVATE, REST_JSONCONVERT_FIELD_PREFIX + i, jsonConvertDesc, null, null);
            fv.visitEnd();
        }

        //classMap.put("mappings", mappingMaps); //不显示太多信息
        { //toString函数
            mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "toString", "()Ljava/lang/String;", null, null));
            //mv.setDebug(true);
            mv.visitLdcInsn(JsonConvert.root().convertTo(classMap));
            mv.visitInsn(ARETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }

        cw.visitEnd();
        Class<?> newClazz = new RestClassLoader(loader).loadClass(newDynName.replace('/', '.'), cw.toByteArray());
        try {
            T obj = ((Class<T>) newClazz).getDeclaredConstructor().newInstance();
            for (Map.Entry<String, org.redkale.util.Attribute> en : restAttributes.entrySet()) {
                Field attrField = newClazz.getDeclaredField(en.getKey());
                attrField.setAccessible(true);
                attrField.set(obj, en.getValue());
            }
            for (Map.Entry<String, java.lang.reflect.Type> en : bodyTypes.entrySet()) {
                Field genField = newClazz.getDeclaredField(en.getKey());
                genField.setAccessible(true);
                genField.set(obj, en.getValue());
            }
            for (int i = 0; i < restConverts.size(); i++) {
                Field genField = newClazz.getDeclaredField(REST_JSONCONVERT_FIELD_PREFIX + (i + 1));
                genField.setAccessible(true);
                genField.set(obj, createJsonConvert(restConverts.get(i)));
            }
            Field typesfield = newClazz.getDeclaredField(REST_PARAMTYPES_FIELD_NAME);
            typesfield.setAccessible(true);
            java.lang.reflect.Type[][] paramtypeArray = new java.lang.reflect.Type[paramtypes.size()][];
            paramtypeArray = paramtypes.toArray(paramtypeArray);
            typesfield.set(obj, paramtypeArray);

            return obj;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static boolean checkName(String name) {  //不能含特殊字符
        if (name.isEmpty()) return true;
        if (name.charAt(0) >= '0' && name.charAt(0) <= '9') return false;
        for (char ch : name.toCharArray()) {
            if (!((ch >= '0' && ch <= '9') || ch == '_' || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))) { //不能含特殊字符
                return false;
            }
        }
        return true;
    }

    private static void pushInt(MethodDebugVisitor mv, int num) {
        if (num < 6) {
            mv.visitInsn(ICONST_0 + num);
        } else if (num <= Byte.MAX_VALUE) {
            mv.visitIntInsn(BIPUSH, num);
        } else if (num <= Short.MAX_VALUE) {
            mv.visitIntInsn(SIPUSH, num);
        } else {
            mv.visitLdcInsn(num);
        }
    }

    private static void pushInt(MethodVisitor mv, int num) {
        if (num < 6) {
            mv.visitInsn(ICONST_0 + num);
        } else if (num <= Byte.MAX_VALUE) {
            mv.visitIntInsn(BIPUSH, num);
        } else if (num <= Short.MAX_VALUE) {
            mv.visitIntInsn(SIPUSH, num);
        } else {
            mv.visitLdcInsn(num);
        }
    }

    private static class RestClassLoader extends ClassLoader {

        public RestClassLoader(ClassLoader parent) {
            super(parent);
        }

        public final Class<?> loadClass(String name, byte[] b) {
            return defineClass(name, b, 0, b.length);
        }
    }

    private static class MappingEntry {

        private static final RestMapping DEFAULT__MAPPING;

        static {
            try {
                DEFAULT__MAPPING = MappingEntry.class.getDeclaredMethod("mapping").getAnnotation(RestMapping.class);
            } catch (Exception e) {
                throw new Error(e);
            }
        }

        public MappingEntry(int methodidx, RestMapping mapping, final String defmodulename, Method method) {
            if (mapping == null) mapping = DEFAULT__MAPPING;
            this.methodidx = methodidx;
            this.ignore = mapping.ignore();
            String n = mapping.name();
            if (n.isEmpty()) {
                String t = method.getName();
                int pos = t.indexOf(defmodulename);
                n = pos > 0 ? t.substring(0, pos) : t;
            }
            this.name = n;
            this.mappingMethod = method;
            this.methods = mapping.methods();
            this.auth = mapping.auth();
            this.actionid = mapping.actionid();
            this.cacheseconds = mapping.cacheseconds();
            this.comment = mapping.comment();
        }

        public final int methodidx; // _paramtypes 的下标，从0开始

        public final Method mappingMethod;

        public final boolean ignore;

        public final String name;

        public final String comment;

        public final String[] methods;

        public final boolean auth;

        public final int actionid;

        public final int cacheseconds;

        @RestMapping()
        void mapping() { //用于获取Mapping 默认值
        }

        @Override
        public int hashCode() {
            return this.name.hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) return true;
            if (obj == null || getClass() != obj.getClass()) return false;
            return this.name.equals(((MappingEntry) obj).name);
        }

    }
}
