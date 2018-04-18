/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.sncp;

import org.redkale.asm.MethodDebugVisitor;
import static org.redkale.net.sncp.SncpRequest.DEFAULT_HEADER;
import java.io.*;
import java.lang.annotation.*;
import java.lang.reflect.*;
import java.nio.*;
import java.nio.channels.CompletionHandler;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.*;
import java.util.logging.*;
import javax.annotation.*;
import org.redkale.asm.*;
import static org.redkale.asm.ClassWriter.COMPUTE_FRAMES;
import static org.redkale.asm.Opcodes.*;
import org.redkale.asm.Type;
import org.redkale.convert.bson.*;
import org.redkale.net.sncp.SncpAsyncHandler.DefaultSncpAsyncHandler;
import org.redkale.service.*;
import org.redkale.util.*;
import org.redkale.service.RpcCall;

/**
 *
 *
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public final class SncpDynServlet extends SncpServlet {

    private static volatile int maxClassNameLength = 0;

    private static volatile int maxNameLength = 0;

    private static final Logger logger = Logger.getLogger(SncpDynServlet.class.getSimpleName());

    private final DLong serviceid;

    private final HashMap<DLong, SncpServletAction> actions = new HashMap<>();

    private Supplier<ByteBuffer> bufferSupplier;

    public SncpDynServlet(final BsonConvert convert, final String serviceName, final Class serviceOrSourceType, final Service service) {
        super(serviceName, serviceOrSourceType, service);
        this.serviceid = Sncp.hash(type.getName() + ':' + serviceName);
        Set<DLong> actionids = new HashSet<>();
        for (java.lang.reflect.Method method : service.getClass().getMethods()) {
            if (method.isSynthetic()) continue;
            if (Modifier.isStatic(method.getModifiers())) continue;
            if (Modifier.isFinal(method.getModifiers())) continue;
            if (method.getName().equals("getClass") || method.getName().equals("toString")) continue;
            if (method.getName().equals("equals") || method.getName().equals("hashCode")) continue;
            if (method.getName().equals("notify") || method.getName().equals("notifyAll") || method.getName().equals("wait")) continue;
            if (method.getName().equals("init") || method.getName().equals("destroy")) continue;
            //if (method.getName().equals("version") || method.getName().equals("name")) continue;
            final DLong actionid = Sncp.hash(method);
            SncpServletAction action = SncpServletAction.create(service, actionid, method);
            action.convert = convert;
            if (actionids.contains(actionid)) {
                throw new RuntimeException(type.getName() + " have action(Method=" + method + ", actionid=" + actionid + ") same to (" + actions.get(actionid).method + ")");
            }
            actions.put(actionid, action);
            actionids.add(actionid);
        }
        maxNameLength = Math.max(maxNameLength, serviceName.length() + 1);
        maxClassNameLength = Math.max(maxClassNameLength, type.getName().length());
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.getClass().getSimpleName()).append(" (type=").append(type.getName());
        int len = maxClassNameLength - type.getName().length();
        for (int i = 0; i < len; i++) {
            sb.append(' ');
        }
        sb.append(", serviceid=").append(serviceid).append(", name='").append(serviceName).append("'");
        for (int i = 0; i < maxNameLength - serviceName.length(); i++) {
            sb.append(' ');
        }
        sb.append(", actions.size=").append(actions.size() > 9 ? "" : " ").append(actions.size()).append(")");
        return sb.toString();
    }

    @Override
    public DLong getServiceid() {
        return serviceid;
    }

    @Override
    public int compareTo(SncpServlet o0) {
        if (!(o0 instanceof SncpDynServlet)) return 1;
        SncpDynServlet o = (SncpDynServlet) o0;
        int rs = this.type.getName().compareTo(o.type.getName());
        if (rs == 0) rs = this.serviceName.compareTo(o.serviceName);
        return rs;
    }

    @Override
    @SuppressWarnings("unchecked")
    public void execute(SncpRequest request, SncpResponse response) throws IOException {
        if (bufferSupplier == null) {
            bufferSupplier = request.getContext().getBufferSupplier();
        }
        final SncpServletAction action = actions.get(request.getActionid());
        //logger.log(Level.FINEST, "sncpdyn.execute: " + request + ", " + (action == null ? "null" : action.method));
        if (action == null) {
            response.finish(SncpResponse.RETCODE_ILLACTIONID, null);  //无效actionid
        } else {
            BsonWriter out = action.convert.pollBsonWriter(bufferSupplier);
            out.writeTo(DEFAULT_HEADER);
            BsonReader in = action.convert.pollBsonReader();
            SncpAsyncHandler handler = null;
            try {
                if (action.handlerFuncParamIndex >= 0) {
                    if (action.handlerFuncParamClass == CompletionHandler.class) {
                        handler = new DefaultSncpAsyncHandler(action, in, out, request, response);
                    } else {
                        Creator<SncpAsyncHandler> creator = action.handlerCreator;
                        if (creator == null) {
                            creator = SncpAsyncHandler.Factory.createCreator(action.handlerFuncParamClass);
                            action.handlerCreator = creator;
                        }
                        handler = creator.create(new DefaultSncpAsyncHandler(action, in, out, request, response));
                    }
                } else if (action.boolReturnTypeFuture) {
                    handler = new DefaultSncpAsyncHandler(action, in, out, request, response);
                }
                in.setBytes(request.getBody());
                action.action(in, out, handler);
                if (handler == null) {
                    response.finish(0, out);
                    action.convert.offerBsonReader(in);
                    action.convert.offerBsonWriter(out);
                } else if (action.boolReturnTypeFuture) {
                    CompletableFuture future = handler.sncp_getFuture();
                    if (future == null) {
                        action._callParameter(out, handler.sncp_getParams());
                        action.convert.convertTo(out, Object.class, null);
                    } else {
                        Object[] sncpParams = handler.sncp_getParams();
                        future.whenComplete((v, e) -> {
                            if (e != null) {
                                response.getContext().getLogger().log(Level.SEVERE, "sncp CompleteAsync error(" + request + ")", e);
                                response.finish(SncpResponse.RETCODE_THROWEXCEPTION, null);
                                return;
                            }
                            action._callParameter(out, sncpParams);
                            action.convert.convertTo(out, Object.class, v);
                            response.finish(0, out);
                            action.convert.offerBsonReader(in);
                            action.convert.offerBsonWriter(out);
                        });
                    }
                }
            } catch (Throwable t) {
                response.getContext().getLogger().log(Level.SEVERE, "sncp execute error(" + request + ")", t);
                response.finish(SncpResponse.RETCODE_THROWEXCEPTION, null);
            }
        }
    }

    public static abstract class SncpServletAction {

        public Method method;

        public Creator<SncpAsyncHandler> handlerCreator;

        @Resource
        protected BsonConvert convert;

        protected org.redkale.util.Attribute[] paramAttrs; // 为null表示无RpcCall处理，index=0固定为null, 其他为参数标记的RpcCall回调方法

        protected java.lang.reflect.Type[] paramTypes;  //index=0表示返回参数的type， void的返回参数类型为null

        protected int handlerFuncParamIndex = -1;  //handlerFuncParamIndex>=0表示存在CompletionHandler参数

        protected boolean boolReturnTypeFuture = false; // 返回结果类型是否为 CompletableFuture

        protected Class handlerFuncParamClass; //CompletionHandler参数的类型

        public abstract void action(final BsonReader in, final BsonWriter out, final SncpAsyncHandler handler) throws Throwable;

        //只有同步方法才调用 (没有CompletionHandler、CompletableFuture)
        public final void _callParameter(final BsonWriter out, final Object... params) {
            if (paramAttrs != null) {
                for (int i = 1; i < paramAttrs.length; i++) {
                    org.redkale.util.Attribute attr = paramAttrs[i];
                    if (attr == null) continue;
                    out.writeByte((byte) i);
                    convert.convertTo(out, attr.type(), attr.get(params[i - 1]));
                }
            }
            out.writeByte((byte) 0);
        }

        /**
         * <blockquote><pre>
         *  public class TestService implements Service {
         *
         *      public boolean change(TestBean bean, String name, int id) {
         *          return false;
         *      }
         *
         *      public void insert(CompletionHandler&#60;Boolean, TestBean&#62; handler, TestBean bean, String name, int id) {
         *      }
         *
         *      public void update(long show, short v2, CompletionHandler&#60;Boolean, TestBean&#62; handler, TestBean bean, String name, int id) {
         *      }
         *
         *      public CompletableFuture&#60;String&#62; changeName(TestBean bean, String name, int id) {
         *          return null;
         *      }
         * }
         *
         *
         * class DynActionTestService_change extends SncpServletAction {
         *
         *      public TestService service;
         *
         *      &#064;Override
         *      public void action(BsonReader in, BsonWriter out, SncpAsyncHandler handler) throws Throwable {
         *          TestBean arg1 = convert.convertFrom(paramTypes[1], in);
         *          String arg2 = convert.convertFrom(paramTypes[2], in);
         *          int arg3 = convert.convertFrom(paramTypes[3], in);
         *          Object rs = service.change(arg1, arg2, arg3);
         *          _callParameter(out, arg1, arg2, arg3);
         *          convert.convertTo(out, paramTypes[0], rs);
         *      }
         * }
         *
         * class DynActionTestService_insert extends SncpServletAction {
         *
         *      public TestService service;
         *
         *      &#064;Override
         *      public void action(BsonReader in, BsonWriter out, SncpAsyncHandler handler) throws Throwable {
         *          SncpAsyncHandler arg0 = handler;
         *          convert.convertFrom(CompletionHandler.class, in);
         *          TestBean arg1 = convert.convertFrom(paramTypes[2], in);
         *          String arg2 = convert.convertFrom(paramTypes[3], in);
         *          int arg3 = convert.convertFrom(paramTypes[4], in);
         *          handler.sncp_setParams(arg0, arg1, arg2, arg3);
         *          service.insert(arg0, arg1, arg2, arg3);
         *       }
         * }
         *
         * class DynActionTestService_update extends SncpServletAction {
         *
         *      public TestService service;
         *
         *      &#064;Override
         *      public void action(BsonReader in, BsonWriter out, SncpAsyncHandler handler) throws Throwable {
         *          long a1 = convert.convertFrom(paramTypes[1], in);
         *          short a2 = convert.convertFrom(paramTypes[2], in);
         *          SncpAsyncHandler a3 = handler;
         *          convert.convertFrom(CompletionHandler.class, in);
         *          TestBean arg1 = convert.convertFrom(paramTypes[4], in);
         *          String arg2 = convert.convertFrom(paramTypes[5], in);
         *          int arg3 = convert.convertFrom(paramTypes[6], in);
         *          handler.sncp_setParams(a1, a2, a3, arg1, arg2, arg3);
         *          service.update(a1, a2, a3, arg1, arg2, arg3);
         *      }
         * }
         *
         *
         * class DynActionTestService_changeName extends SncpServletAction {
         *
         *      public TestService service;
         *
         *      &#064;Override
         *      public void action(final BsonReader in, final BsonWriter out, final SncpAsyncHandler handler) throws Throwable {
         *          TestBean arg1 = convert.convertFrom(paramTypes[1], in);
         *          String arg2 = convert.convertFrom(paramTypes[2], in);
         *          int arg3 = convert.convertFrom(paramTypes[3], in);
         *          handler.sncp_setParams(arg1, arg2, arg3);
         *          CompletableFuture future = service.changeName(arg1, arg2, arg3);
         *          handler.sncp_setFuture(future);
         *      }
         * }
         *
         * </pre></blockquote>
         *
         * @param service  Service
         * @param actionid 操作ID
         * @param method   方法
         *
         * @return SncpServletAction
         */
        @SuppressWarnings("unchecked")
        public static SncpServletAction create(final Service service, final DLong actionid, final Method method) {
            final Class serviceClass = service.getClass();
            final String supDynName = SncpServletAction.class.getName().replace('.', '/');
            final String serviceName = serviceClass.getName().replace('.', '/');
            final String convertName = BsonConvert.class.getName().replace('.', '/');
            final String handlerName = SncpAsyncHandler.class.getName().replace('.', '/');
            final String asyncHandlerDesc = Type.getDescriptor(SncpAsyncHandler.class);
            final String convertReaderDesc = Type.getDescriptor(BsonReader.class);
            final String convertWriterDesc = Type.getDescriptor(BsonWriter.class);
            final String serviceDesc = Type.getDescriptor(serviceClass);
            final boolean boolReturnTypeFuture = CompletableFuture.class.isAssignableFrom(method.getReturnType());
            String newDynName = serviceName.substring(0, serviceName.lastIndexOf('/') + 1)
                + "DynAction" + serviceClass.getSimpleName() + "_" + method.getName() + "_" + actionid;
            while (true) {
                try {
                    Thread.currentThread().getContextClassLoader().loadClass(newDynName.replace('/', '.'));
                    newDynName += "_";
                } catch (Throwable ex) {
                    break;
                }
            }
            //-------------------------------------------------------------
            ClassWriter cw = new ClassWriter(COMPUTE_FRAMES);
            FieldVisitor fv;
            MethodDebugVisitor mv;

            cw.visit(V1_8, ACC_PUBLIC + ACC_FINAL + ACC_SUPER, newDynName, null, supDynName, null);

            {
                {
                    fv = cw.visitField(ACC_PUBLIC, "service", serviceDesc, null, null);
                    fv.visitEnd();
                }
                fv.visitEnd();
            }
            {  // constructor方法
                mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null));
                mv.visitVarInsn(ALOAD, 0);
                mv.visitMethodInsn(INVOKESPECIAL, supDynName, "<init>", "()V", false);
                mv.visitInsn(RETURN);
                mv.visitMaxs(1, 1);
                mv.visitEnd();
            }
            String convertFromDesc = "(Ljava/lang/reflect/Type;" + convertReaderDesc + ")Ljava/lang/Object;";
            try {
                convertFromDesc = Type.getMethodDescriptor(BsonConvert.class.getMethod("convertFrom", java.lang.reflect.Type.class, BsonReader.class));
            } catch (Exception ex) {
                throw new RuntimeException(ex); //不可能会发生
            }
            int handlerFuncIndex = -1;
            Class handlerFuncClass = null;
            { // action方法
                mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "action", "(" + convertReaderDesc + convertWriterDesc + asyncHandlerDesc + ")V", null, new String[]{"java/lang/Throwable"}));
                //mv.setDebug(true);
                int iconst = ICONST_1;
                int intconst = 1;
                int store = 4; //action的参数个数+1
                final Class[] paramClasses = method.getParameterTypes();
                int[][] codes = new int[paramClasses.length][2];
                for (int i = 0; i < paramClasses.length; i++) { //反序列化方法的每个参数
                    if (CompletionHandler.class.isAssignableFrom(paramClasses[i])) {
                        if (boolReturnTypeFuture) {
                            throw new RuntimeException(method + " have both CompletionHandler and CompletableFuture");
                        }
                        if (handlerFuncIndex >= 0) {
                            throw new RuntimeException(method + " have more than one CompletionHandler type parameter");
                        }
                        Sncp.checkAsyncModifier(paramClasses[i], method);
                        handlerFuncIndex = i;
                        handlerFuncClass = paramClasses[i];
                        mv.visitVarInsn(ALOAD, 3);
                        mv.visitTypeInsn(CHECKCAST, paramClasses[i].getName().replace('.', '/'));
                        mv.visitVarInsn(ASTORE, store);
                        codes[i] = new int[]{ALOAD, store};
                        store++;
                        iconst++;
                        intconst++;
                        mv.visitVarInsn(ALOAD, 0);
                        mv.visitFieldInsn(GETFIELD, newDynName, "convert", Type.getDescriptor(BsonConvert.class));
                        mv.visitLdcInsn(Type.getType(Type.getDescriptor(CompletionHandler.class)));
                        mv.visitVarInsn(ALOAD, 1);
                        mv.visitMethodInsn(INVOKEVIRTUAL, convertName, "convertFrom", convertFromDesc, false);
                        mv.visitInsn(POP);
                        continue;
                    }
                    mv.visitVarInsn(ALOAD, 0);
                    mv.visitFieldInsn(GETFIELD, newDynName, "convert", Type.getDescriptor(BsonConvert.class));
                    mv.visitVarInsn(ALOAD, 0);
                    mv.visitFieldInsn(GETFIELD, newDynName, "paramTypes", "[Ljava/lang/reflect/Type;");

                    if (intconst < 6) {
                        mv.visitInsn(ICONST_0 + intconst);
                    } else if (iconst <= Byte.MAX_VALUE) {
                        mv.visitIntInsn(BIPUSH, intconst);
                    } else if (iconst <= Short.MAX_VALUE) {
                        mv.visitIntInsn(SIPUSH, intconst);
                    } else {
                        mv.visitLdcInsn(intconst);
                    }
                    mv.visitInsn(AALOAD);
                    mv.visitVarInsn(ALOAD, 1);

                    mv.visitMethodInsn(INVOKEVIRTUAL, convertName, "convertFrom", convertFromDesc, false);
                    int load = ALOAD;
                    int v = 0;
                    if (paramClasses[i].isPrimitive()) {
                        int storecode = ISTORE;
                        load = ILOAD;
                        if (paramClasses[i] == long.class) {
                            storecode = LSTORE;
                            load = LLOAD;
                            v = 1;
                        } else if (paramClasses[i] == float.class) {
                            storecode = FSTORE;
                            load = FLOAD;
                            v = 1;
                        } else if (paramClasses[i] == double.class) {
                            storecode = DSTORE;
                            load = DLOAD;
                            v = 1;
                        }
                        Class bigPrimitiveClass = Array.get(Array.newInstance(paramClasses[i], 1), 0).getClass();
                        String bigPrimitiveName = bigPrimitiveClass.getName().replace('.', '/');
                        try {
                            Method pm = bigPrimitiveClass.getMethod(paramClasses[i].getSimpleName() + "Value");
                            mv.visitTypeInsn(CHECKCAST, bigPrimitiveName);
                            mv.visitMethodInsn(INVOKEVIRTUAL, bigPrimitiveName, pm.getName(), Type.getMethodDescriptor(pm), false);
                        } catch (Exception ex) {
                            throw new RuntimeException(ex); //不可能会发生
                        }
                        mv.visitVarInsn(storecode, store);
                    } else {
                        mv.visitTypeInsn(CHECKCAST, paramClasses[i].getName().replace('.', '/'));
                        mv.visitVarInsn(ASTORE, store);  //
                    }
                    codes[i] = new int[]{load, store};
                    store += v;
                    iconst++;
                    intconst++;
                    store++;
                }
                if (boolReturnTypeFuture || handlerFuncIndex >= 0) {  //调用SncpAsyncHandler.setParams(Object... params)
                    mv.visitVarInsn(ALOAD, 3);
                    if (paramClasses.length < 6) {
                        mv.visitInsn(ICONST_0 + paramClasses.length);
                    } else if (paramClasses.length <= Byte.MAX_VALUE) {
                        mv.visitIntInsn(BIPUSH, paramClasses.length);
                    } else if (paramClasses.length <= Short.MAX_VALUE) {
                        mv.visitIntInsn(SIPUSH, paramClasses.length);
                    } else {
                        mv.visitLdcInsn(paramClasses.length);
                    }

                    mv.visitTypeInsn(ANEWARRAY, "java/lang/Object");
                    int insn = 3; //action的参数个数
                    for (int j = 0; j < paramClasses.length; j++) {
                        final Class pt = paramClasses[j];
                        mv.visitInsn(DUP);
                        insn++;
                        if (j < 6) {
                            mv.visitInsn(ICONST_0 + j);
                        } else if (j <= Byte.MAX_VALUE) {
                            mv.visitIntInsn(BIPUSH, j);
                        } else if (j <= Short.MAX_VALUE) {
                            mv.visitIntInsn(SIPUSH, j);
                        } else {
                            mv.visitLdcInsn(j);
                        }
                        if (pt.isPrimitive()) {
                            if (pt == long.class) {
                                mv.visitVarInsn(LLOAD, insn++);
                            } else if (pt == float.class) {
                                mv.visitVarInsn(FLOAD, insn++);
                            } else if (pt == double.class) {
                                mv.visitVarInsn(DLOAD, insn++);
                            } else {
                                mv.visitVarInsn(ILOAD, insn);
                            }
                            Class bigclaz = java.lang.reflect.Array.get(java.lang.reflect.Array.newInstance(pt, 1), 0).getClass();
                            mv.visitMethodInsn(INVOKESTATIC, bigclaz.getName().replace('.', '/'), "valueOf", "(" + Type.getDescriptor(pt) + ")" + Type.getDescriptor(bigclaz), false);
                        } else {
                            mv.visitVarInsn(ALOAD, insn);
                        }
                        mv.visitInsn(AASTORE);
                    }
                    mv.visitMethodInsn(INVOKEINTERFACE, handlerName, "sncp_setParams", "([Ljava/lang/Object;)V", true);
                }
                {  //调用service
                    mv.visitVarInsn(ALOAD, 0);
                    mv.visitFieldInsn(GETFIELD, newDynName, "service", serviceDesc);
                    for (int[] j : codes) {
                        mv.visitVarInsn(j[0], j[1]);
                    }
                    mv.visitMethodInsn(INVOKEVIRTUAL, serviceName, method.getName(), Type.getMethodDescriptor(method), false);
                }

                final Class returnClass = method.getReturnType();
                if (returnClass != void.class) {
                    if (returnClass.isPrimitive()) {
                        Class bigClass = Array.get(Array.newInstance(returnClass, 1), 0).getClass();
                        try {
                            Method vo = bigClass.getMethod("valueOf", returnClass);
                            mv.visitMethodInsn(INVOKESTATIC, bigClass.getName().replace('.', '/'), vo.getName(), Type.getMethodDescriptor(vo), false);
                        } catch (Exception ex) {
                            throw new RuntimeException(ex); //不可能会发生
                        }
                    }
                    mv.visitVarInsn(ASTORE, store);  //11
                    if (boolReturnTypeFuture) {
                        mv.visitVarInsn(ALOAD, 3);
                        mv.visitVarInsn(ALOAD, store);
                        mv.visitMethodInsn(INVOKEINTERFACE, handlerName, "sncp_setFuture", "(Ljava/util/concurrent/CompletableFuture;)V", true);
                    }
                }
                if (!boolReturnTypeFuture && handlerFuncIndex < 0) { //同步方法
                    //------------------------- _callParameter 方法 --------------------------------
                    mv.visitVarInsn(ALOAD, 0);
                    mv.visitVarInsn(ALOAD, 2);
                    if (paramClasses.length < 6) {  //参数总数量
                        mv.visitInsn(ICONST_0 + paramClasses.length);
                    } else if (paramClasses.length <= Byte.MAX_VALUE) {
                        mv.visitIntInsn(BIPUSH, paramClasses.length);
                    } else if (paramClasses.length <= Short.MAX_VALUE) {
                        mv.visitIntInsn(SIPUSH, paramClasses.length);
                    } else {
                        mv.visitLdcInsn(paramClasses.length);
                    }
                    mv.visitTypeInsn(ANEWARRAY, "java/lang/Object");
                    int insn = 3;//action的参数个数
                    for (int j = 0; j < paramClasses.length; j++) {
                        final Class pt = paramClasses[j];
                        mv.visitInsn(DUP);
                        insn++;
                        if (j < 6) {
                            mv.visitInsn(ICONST_0 + j);
                        } else if (j <= Byte.MAX_VALUE) {
                            mv.visitIntInsn(BIPUSH, j);
                        } else if (j <= Short.MAX_VALUE) {
                            mv.visitIntInsn(SIPUSH, j);
                        } else {
                            mv.visitLdcInsn(j);
                        }
                        if (pt.isPrimitive()) {
                            if (pt == long.class) {
                                mv.visitVarInsn(LLOAD, insn++);
                            } else if (pt == float.class) {
                                mv.visitVarInsn(FLOAD, insn++);
                            } else if (pt == double.class) {
                                mv.visitVarInsn(DLOAD, insn++);
                            } else {
                                mv.visitVarInsn(ILOAD, insn);
                            }
                            Class bigclaz = java.lang.reflect.Array.get(java.lang.reflect.Array.newInstance(pt, 1), 0).getClass();
                            mv.visitMethodInsn(INVOKESTATIC, bigclaz.getName().replace('.', '/'), "valueOf", "(" + Type.getDescriptor(pt) + ")" + Type.getDescriptor(bigclaz), false);
                        } else {
                            mv.visitVarInsn(ALOAD, insn);
                        }
                        mv.visitInsn(AASTORE);
                    }
                    mv.visitMethodInsn(INVOKEVIRTUAL, newDynName, "_callParameter", "(" + convertWriterDesc + "[Ljava/lang/Object;)V", false);
                }
                //-------------------------直接返回  或者  调用convertTo方法 --------------------------------
                int maxStack = codes.length > 0 ? codes[codes.length - 1][1] : 1;
                if (boolReturnTypeFuture || returnClass == void.class) { //返回
                    mv.visitInsn(RETURN);
                    maxStack = 8;
                } else {  //同步方法调用
                    mv.visitVarInsn(ALOAD, 0);
                    mv.visitFieldInsn(GETFIELD, newDynName, "convert", Type.getDescriptor(BsonConvert.class));
                    mv.visitVarInsn(ALOAD, 2);
                    mv.visitVarInsn(ALOAD, 0);
                    mv.visitFieldInsn(GETFIELD, newDynName, "paramTypes", "[Ljava/lang/reflect/Type;");
                    mv.visitInsn(ICONST_0);
                    mv.visitInsn(AALOAD);
                    mv.visitVarInsn(ALOAD, store);
                    mv.visitMethodInsn(INVOKEVIRTUAL, convertName, "convertTo", "(" + convertWriterDesc + "Ljava/lang/reflect/Type;Ljava/lang/Object;)V", false);
                    mv.visitInsn(RETURN);
                    store++;
                }
                mv.visitMaxs(maxStack, store);
                mv.visitEnd();
            }
            cw.visitEnd();

            byte[] bytes = cw.toByteArray();
            Class<?> newClazz = new ClassLoader(serviceClass.getClassLoader()) {
                public final Class<?> loadClass(String name, byte[] b) {
                    return defineClass(name, b, 0, b.length);
                }
            }.loadClass(newDynName.replace('/', '.'), bytes);
            try {
                SncpServletAction instance = (SncpServletAction) newClazz.getDeclaredConstructor().newInstance();
                instance.method = method;
                java.lang.reflect.Type[] ptypes = TypeToken.getGenericType(method.getGenericParameterTypes(), serviceClass);
                java.lang.reflect.Type[] types = new java.lang.reflect.Type[ptypes.length + 1];
                java.lang.reflect.Type rt = TypeToken.getGenericType(method.getGenericReturnType(), serviceClass);
                types[0] = rt;
                System.arraycopy(ptypes, 0, types, 1, ptypes.length);
                instance.paramTypes = types;
                instance.handlerFuncParamIndex = handlerFuncIndex;
                instance.handlerFuncParamClass = handlerFuncClass;
                instance.boolReturnTypeFuture = boolReturnTypeFuture;

                org.redkale.util.Attribute[] atts = new org.redkale.util.Attribute[ptypes.length + 1];
                Annotation[][] anns = method.getParameterAnnotations();
                boolean hasattr = false;
                for (int i = 0; i < anns.length; i++) {
                    if (anns[i].length > 0) {
                        for (Annotation ann : anns[i]) {
                            if (ann.annotationType() == RpcCall.class) {
                                try {
                                    atts[i + 1] = ((RpcCall) ann).value().getDeclaredConstructor().newInstance();
                                    hasattr = true;
                                } catch (Exception e) {
                                    logger.log(Level.SEVERE, RpcCall.class.getSimpleName() + ".attribute cannot a newInstance for" + method, e);
                                }
                                break;
                            }
                        }
                    }
                }
                if (hasattr) instance.paramAttrs = atts;
                newClazz.getField("service").set(instance, service);
                return instance;
            } catch (Exception ex) {
                throw new RuntimeException(ex); //不可能会发生
            }
        }
    }

}
