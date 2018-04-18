/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.sncp;

import org.redkale.asm.MethodDebugVisitor;
import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.CompletionHandler;
import java.security.*;
import java.util.*;
import javax.annotation.Resource;
import static org.redkale.asm.ClassWriter.COMPUTE_FRAMES;
import org.redkale.asm.*;
import static org.redkale.asm.Opcodes.*;
import org.redkale.asm.Type;
import org.redkale.net.TransportFactory;
import org.redkale.net.sncp.SncpClient.SncpAction;
import org.redkale.service.*;
import org.redkale.util.*;

/**
 * Service Node Communicate Protocol
 * 生成Service的本地模式或远程模式Service-Class的工具类
 *
 *
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public abstract class Sncp {

    public static final ByteBuffer PING_BUFFER = ByteBuffer.wrap("PING".getBytes()).asReadOnlyBuffer();

    public static final ByteBuffer PONG_BUFFER = ByteBuffer.wrap("PONG".getBytes()).asReadOnlyBuffer();

    static final String FIELDPREFIX = "_redkale";

    static final String LOCALPREFIX = "_DynLocal";

    static final String REMOTEPREFIX = "_DynRemote";

    private static final MessageDigest md5;

    static {  //64进制
        MessageDigest d = null;
        try {
            d = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException ex) {
            ex.printStackTrace();
        }
        md5 = d;
    }

    private Sncp() {
    }

    public static DLong hash(final java.lang.reflect.Method method) {
        if (method == null) return DLong.ZERO;
        StringBuilder sb = new StringBuilder(); //不能使用method.toString() 因为包含declaringClass信息导致接口与实现类的方法hash不一致
        sb.append(method.getReturnType().getName()).append(' ');
        sb.append(method.getName());
        sb.append('(');
        boolean first = true;
        for (Class pt : method.getParameterTypes()) {
            if (!first) sb.append(',');
            sb.append(pt.getName());
            first = false;
        }
        sb.append(')');
        return hash(sb.toString());
    }

    /**
     * 对类名或者name字符串进行hash。
     *
     * @param name String
     *
     * @return hash值
     */
    public static DLong hash(final String name) {
        if (name == null || name.isEmpty()) return DLong.ZERO;
        byte[] bytes = name.trim().getBytes();
        synchronized (md5) {
            bytes = md5.digest(bytes);
        }
        return DLong.create(bytes);
    }

    public static boolean isRemote(Service service) {
        SncpDyn dyn = service.getClass().getAnnotation(SncpDyn.class);
        return dyn != null && dyn.remote();
    }

    public static boolean isSncpDyn(Service service) {
        return service.getClass().getAnnotation(SncpDyn.class) != null;
    }

    public static String getResourceName(Service service) {
        if (service == null) return null;
        Resource res = service.getClass().getAnnotation(Resource.class);
        return res == null ? null : res.name();
    }

    public static Class getServiceType(Service service) {
        ResourceType rt = service.getClass().getAnnotation(ResourceType.class);
        return rt == null ? service.getClass() : rt.value();
    }

    public static Class getResourceType(Service service) {
        if (service == null) return null;
        ResourceType type = service.getClass().getAnnotation(ResourceType.class);
        return type == null ? getServiceType(service) : type.value();
    }

    public static AnyValue getConf(Service service) {
        if (service == null) return null;
        try {
            Field ts = service.getClass().getDeclaredField(FIELDPREFIX + "_conf");
            ts.setAccessible(true);
            return (AnyValue) ts.get(service);
        } catch (Exception e) {
            throw new RuntimeException(service + " not found " + FIELDPREFIX + "_conf");
        }
    }

    public static SncpClient getSncpClient(Service service) {
        if (service == null) return null;
        try {
            Field ts = service.getClass().getDeclaredField(FIELDPREFIX + "_client");
            ts.setAccessible(true);
            return (SncpClient) ts.get(service);
        } catch (Exception e) {
            throw new RuntimeException(service + " not found " + FIELDPREFIX + "_client");
        }
    }

    static void checkAsyncModifier(Class param, Method method) {
        if (param == CompletionHandler.class) return;
        if (Modifier.isFinal(param.getModifiers())) {
            throw new RuntimeException("CompletionHandler Type Parameter on {" + method + "} cannot final modifier");
        }
        if (!Modifier.isPublic(param.getModifiers())) {
            throw new RuntimeException("CompletionHandler Type Parameter on {" + method + "} must be public modifier");
        }
        if (param.isInterface()) return;
        boolean constructorflag = false;
        for (Constructor c : param.getDeclaredConstructors()) {
            if (c.getParameterCount() == 0) {
                int mod = c.getModifiers();
                if (Modifier.isPublic(mod) || Modifier.isProtected(mod)) {
                    constructorflag = true;
                    break;
                }
            }
        }
        if (param.getDeclaredConstructors().length == 0) constructorflag = true;
        if (!constructorflag) throw new RuntimeException(param + " must have a empty parameter Constructor");
        for (Method m : param.getMethods()) {
            if (m.getName().equals("completed") && Modifier.isFinal(m.getModifiers())) {
                throw new RuntimeException(param + "'s completed method cannot final modifier");
            } else if (m.getName().equals("failed") && Modifier.isFinal(m.getModifiers())) {
                throw new RuntimeException(param + "'s failed method cannot final modifier");
            } else if (m.getName().equals("sncp_getParams") && Modifier.isFinal(m.getModifiers())) {
                throw new RuntimeException(param + "'s sncp_getParams method cannot final modifier");
            } else if (m.getName().equals("sncp_setParams") && Modifier.isFinal(m.getModifiers())) {
                throw new RuntimeException(param + "'s sncp_setParams method cannot final modifier");
            } else if (m.getName().equals("sncp_setFuture") && Modifier.isFinal(m.getModifiers())) {
                throw new RuntimeException(param + "'s sncp_setFuture method cannot final modifier");
            } else if (m.getName().equals("sncp_getFuture") && Modifier.isFinal(m.getModifiers())) {
                throw new RuntimeException(param + "'s sncp_getFuture method cannot final modifier");
            }
        }
    }

    public static String toSimpleString(final Service service, int maxNameLength, int maxClassNameLength) {
        StringBuilder sb = new StringBuilder();
        sb.append(isRemote(service) ? "RemoteService" : "LocalService ");
        int len;
        Class type = getResourceType(service);
        String name = getResourceName(service);
        sb.append("(type= ").append(type.getName());
        len = maxClassNameLength - type.getName().length();
        for (int i = 0; i < len; i++) {
            sb.append(' ');
        }
        sb.append(", name='").append(name).append("'");
        for (int i = 0; i < maxNameLength - name.length(); i++) {
            sb.append(' ');
        }
        sb.append(")");
        return sb.toString();
    }

    /**
     * <blockquote><pre>
     * public class TestService implements Service{
     *
     *      public String findSomeThing(){
     *          return "hello";
     *      }
     *
     *      &#64;RpcMultiRun(selfrun = false)
     *      public void createSomeThing(TestBean bean){
     *          //do something
     *      }
     *
     *      &#64;RpcMultiRun
     *      public String updateSomeThing(String id){
     *          return "hello" + id;
     *      }
     * }
     * </pre></blockquote>
     *
     * <blockquote><pre>
     * &#64;Resource(name = "")
     * &#64;SncpDyn(remote = false)
     * &#64;ResourceType(TestService.class)
     * public final class _DynLocalTestService extends TestService{
     *
     *      private AnyValue _redkale_conf;
     *
     *      private SncpClient _redkale_client;
     *
     *      &#64;Override
     *      public String toString() {
     *          return _redkale_selfstring == null ? super.toString() : _redkale_selfstring;
     *      }
     *
     *      &#64;Override
     *      public void createSomeThing(TestBean bean){
     *          this._redkale_createSomeThing(false, true, true, bean);
     *      }
     *
     *      &#64;SncpDyn(remote = false, index = 0)
     *      public void _redkale_createSomeThing(boolean selfrunnable, boolean samerunnable, boolean diffrunnable, TestBean bean){
     *          if(selfrunnable) super.createSomeThing(bean);
     *          if (_redkale_client== null) return;
     *          if (samerunnable) _redkale_client.remoteSameGroup(0, true, false, false, bean);
     *          if (diffrunnable) _redkale_client.remoteDiffGroup(0, true, true, false, bean);
     *      }
     *
     *      &#64;Override
     *      public String updateSomeThing(String id){
     *          return this._redkale_updateSomeThing(true, true, true, id);
     *      }
     *
     *      &#64;SncpDyn(remote = false, index = 1)
     *      public String _redkale_updateSomeThing(boolean selfrunnable, boolean samerunnable, boolean diffrunnable, String id){
     *          String rs = super.updateSomeThing(id);
     *          if (_redkale_client== null) return rs;
     *          if (samerunnable) _redkale_client.remoteSameGroup(1, true, false, false, id);
     *          if (diffrunnable) _redkale_client.remoteDiffGroup(1, true, true, false, id);
     *          return rs;
     *      }
     * }
     * </pre></blockquote>
     *
     * 创建Service的本地模式Class
     *
     * @param <T>              Service子类
     * @param classLoader      ClassLoader
     * @param name             资源名
     * @param serviceImplClass Service类
     *
     * @return Service实例
     */
    @SuppressWarnings("unchecked")
    protected static <T extends Service> Class<? extends T> createLocalServiceClass(ClassLoader classLoader, final String name, final Class<T> serviceImplClass) {
        if (serviceImplClass == null) return null;
        if (!Service.class.isAssignableFrom(serviceImplClass)) return serviceImplClass;
        ResourceFactory.checkResourceName(name);
        int mod = serviceImplClass.getModifiers();
        if (!java.lang.reflect.Modifier.isPublic(mod)) return serviceImplClass;
        if (java.lang.reflect.Modifier.isAbstract(mod)) return serviceImplClass;
        final List<Method> methods = SncpClient.parseMethod(serviceImplClass);
        final String supDynName = serviceImplClass.getName().replace('.', '/');
        final String clientName = SncpClient.class.getName().replace('.', '/');
        final String resDesc = Type.getDescriptor(Resource.class);
        final String clientDesc = Type.getDescriptor(SncpClient.class);
        final String anyValueDesc = Type.getDescriptor(AnyValue.class);
        final String sncpDynDesc = Type.getDescriptor(SncpDyn.class);
        ClassLoader loader = classLoader == null ? Thread.currentThread().getContextClassLoader() : classLoader;
        String newDynName = supDynName.substring(0, supDynName.lastIndexOf('/') + 1) + LOCALPREFIX + serviceImplClass.getSimpleName();
        if (!name.isEmpty()) {
            boolean normal = true;
            for (char ch : name.toCharArray()) {
                if (!((ch >= '0' && ch <= '9') || ch == '_' || (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))) normal = false;
            }
            if (!normal) throw new RuntimeException(serviceImplClass + "'s resource name is illegal, must be 0-9 _ a-z A-Z");
            newDynName += "_" + (normal ? name : hash(name));
        }
        try {
            return (Class<T>) loader.loadClass(newDynName.replace('/', '.'));
        } catch (Throwable ex) {
        }
        //------------------------------------------------------------------------------
        ClassWriter cw = new ClassWriter(COMPUTE_FRAMES);
        FieldVisitor fv;
        MethodDebugVisitor mv;
        AnnotationVisitor av0;

        cw.visit(V1_8, ACC_PUBLIC + ACC_FINAL + ACC_SUPER, newDynName, null, supDynName, null);
        {
            av0 = cw.visitAnnotation(resDesc, true);
            av0.visit("name", name);
            av0.visitEnd();
        }
        {
            av0 = cw.visitAnnotation(sncpDynDesc, true);
            av0.visit("remote", Boolean.FALSE);
            av0.visitEnd();
        }
        { //给新类加上 原有的Annotation
            for (Annotation ann : serviceImplClass.getAnnotations()) {
                if (ann instanceof Resource || ann instanceof SncpDyn || ann instanceof ResourceType) continue;
                visitAnnotation(cw.visitAnnotation(Type.getDescriptor(ann.annotationType()), true), ann);
            }
        }
        {
            av0 = cw.visitAnnotation(Type.getDescriptor(ResourceType.class), true);
            ResourceType rty = serviceImplClass.getAnnotation(ResourceType.class);
            av0.visit("value", Type.getType(Type.getDescriptor(rty == null ? serviceImplClass : rty.value())));
            av0.visitEnd();
        }
        {
            fv = cw.visitField(ACC_PRIVATE, FIELDPREFIX + "_conf", anyValueDesc, null, null);
            fv.visitEnd();
        }
        {
            fv = cw.visitField(ACC_PRIVATE, FIELDPREFIX + "_client", clientDesc, null, null);
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
        { // toString()
            mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "toString", "()Ljava/lang/String;", null, null));
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, newDynName, FIELDPREFIX + "_client", clientDesc);
            Label l1 = new Label();
            mv.visitJumpInsn(IFNONNULL, l1);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "getClass", "()Ljava/lang/Class;", false);
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Class", "getName", "()Ljava/lang/String;", false);
            Label l2 = new Label();
            mv.visitJumpInsn(GOTO, l2);
            mv.visitLabel(l1);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, newDynName, FIELDPREFIX + "_client", clientDesc);
            mv.visitMethodInsn(INVOKEVIRTUAL, clientName, "toSimpleString", "()Ljava/lang/String;", false);
            mv.visitLabel(l2);
            mv.visitInsn(ARETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }
        int i = - 1;
        for (final Method method : methods) {
            final RpcMultiRun mrun = method.getAnnotation(RpcMultiRun.class);
            if (mrun == null) continue;
            final Class returnType = method.getReturnType();
            final String methodDesc = Type.getMethodDescriptor(method);
            final Class[] paramtypes = method.getParameterTypes();
            final int index = ++i;
            {   //原始方法
                mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC + (method.isVarArgs() ? ACC_VARARGS : 0), method.getName(), methodDesc, null, null));
                //mv.setDebug(true);
                { //给参数加上 Annotation
                    final Annotation[][] anns = method.getParameterAnnotations();
                    for (int k = 0; k < anns.length; k++) {
                        for (Annotation ann : anns[k]) {
                            if (ann instanceof SncpDyn || ann instanceof RpcMultiRun) continue; //必须过滤掉 RpcMultiRun、SncpDyn，否则生成远程模式Service时会出错
                            visitAnnotation(mv.visitParameterAnnotation(k, Type.getDescriptor(ann.annotationType()), true), ann);
                        }
                    }
                }
                mv.visitVarInsn(ALOAD, 0);
                mv.visitInsn(mrun.selfrun() ? ICONST_1 : ICONST_0);
                mv.visitInsn(mrun.samerun() ? ICONST_1 : ICONST_0);
                mv.visitInsn(mrun.diffrun() ? ICONST_1 : ICONST_0);
                int varindex = 0;
                boolean handlerFuncFlag = false;
                for (Class pt : paramtypes) {
                    if (CompletionHandler.class.isAssignableFrom(pt)) {
                        if (handlerFuncFlag) throw new RuntimeException(method + " have more than one CompletionHandler type parameter");
                        checkAsyncModifier(pt, method);
                        handlerFuncFlag = true;
                    }
                    if (pt.isPrimitive()) {
                        if (pt == long.class) {
                            mv.visitVarInsn(LLOAD, ++varindex);
                            ++varindex;
                        } else if (pt == double.class) {
                            mv.visitVarInsn(DLOAD, ++varindex);
                            ++varindex;
                        } else if (pt == float.class) {
                            mv.visitVarInsn(FLOAD, ++varindex);
                        } else {
                            mv.visitVarInsn(ILOAD, ++varindex);
                        }
                    } else {
                        mv.visitVarInsn(ALOAD, ++varindex);
                    }
                }
                mv.visitMethodInsn(INVOKEVIRTUAL, newDynName, FIELDPREFIX + "_" + method.getName(), "(ZZZ" + methodDesc.substring(1), false);
                if (returnType == void.class) {
                    mv.visitInsn(RETURN);
                } else if (returnType.isPrimitive()) {
                    if (returnType == long.class) {
                        mv.visitInsn(LRETURN);
                    } else if (returnType == float.class) {
                        mv.visitInsn(FRETURN);
                    } else if (returnType == double.class) {
                        mv.visitInsn(DRETURN);
                    } else {
                        mv.visitInsn(IRETURN);
                    }
                } else {
                    mv.visitInsn(ARETURN);
                }
                mv.visitMaxs(varindex + 3, varindex + 1);
                mv.visitEnd();
            }
            {  // _方法   _方法比无_方法多了三个参数
                mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC + (method.isVarArgs() ? ACC_VARARGS : 0), FIELDPREFIX + "_" + method.getName(), "(ZZZ" + methodDesc.substring(1), null, null));
                //mv.setDebug(true);  
                { //给参数加上 Annotation
                    final Annotation[][] anns = method.getParameterAnnotations();
                    boolean handlerAttachFlag = false;
                    for (int k = 0; k < anns.length; k++) {
                        for (Annotation ann : anns[k]) {
                            if (ann.annotationType() == RpcAttachment.class) {
                                if (handlerAttachFlag) {
                                    throw new RuntimeException(method + " have more than one @RpcAttachment parameter");
                                }
                                handlerAttachFlag = true;
                            }
                            if (ann instanceof SncpDyn || ann instanceof RpcMultiRun) continue; //必须过滤掉 RpcMultiRun、SncpDyn，否则生成远程模式Service时会出错
                            visitAnnotation(mv.visitParameterAnnotation(k, Type.getDescriptor(ann.annotationType()), true), ann);
                        }
                    }
                }
                av0 = mv.visitAnnotation(sncpDynDesc, true);
                av0.visit("remote", Boolean.FALSE);
                av0.visit("index", index);
                av0.visitEnd();
                //---------------------------- 调用selfrun ---------------------------------
                Label selfLabel = new Label();
                if (returnType == void.class) {  // if
                    mv.visitVarInsn(ILOAD, 1);
                    mv.visitJumpInsn(IFEQ, selfLabel);
                }
                mv.visitVarInsn(ALOAD, 0);
                int varindex = 3; //空3给selfrunnable、samerunnable、diffrunnable
                for (Class pt : paramtypes) {
                    if (pt.isPrimitive()) {
                        if (pt == long.class) {
                            mv.visitVarInsn(LLOAD, ++varindex);
                            ++varindex;
                        } else if (pt == double.class) {
                            mv.visitVarInsn(DLOAD, ++varindex);
                            ++varindex;
                        } else if (pt == float.class) {
                            mv.visitVarInsn(FLOAD, ++varindex);
                        } else {
                            mv.visitVarInsn(ILOAD, ++varindex);
                        }
                    } else {
                        mv.visitVarInsn(ALOAD, ++varindex);
                    }
                }
                mv.visitMethodInsn(INVOKESPECIAL, supDynName, method.getName(), methodDesc, false);
                if (returnType == void.class) {  // end if
                    mv.visitLabel(selfLabel);
                }
                if (returnType == void.class) {
                } else if (returnType.isPrimitive()) {
                    if (returnType == long.class) {
                        mv.visitVarInsn(LSTORE, ++varindex);
                        //++varindex; //多加1
                    } else if (returnType == float.class) {
                        mv.visitVarInsn(FSTORE, ++varindex);
                    } else if (returnType == double.class) {
                        mv.visitVarInsn(DSTORE, ++varindex);
                        //++varindex; //多加1
                    } else {
                        mv.visitVarInsn(ISTORE, ++varindex);
                    }
                } else {
                    mv.visitVarInsn(ASTORE, ++varindex);
                }
                final int rsindex = varindex;  //

                //---------------------------if (_redkale_client== null)  return ----------------------------------
                mv.visitVarInsn(ALOAD, 0);
                mv.visitFieldInsn(GETFIELD, newDynName, FIELDPREFIX + "_client", clientDesc);
                Label clientLabel = new Label();
                mv.visitJumpInsn(IFNONNULL, clientLabel);
                if (returnType == void.class) {
                    mv.visitInsn(RETURN);
                } else if (returnType.isPrimitive()) {
                    if (returnType == long.class) {
                        mv.visitVarInsn(LLOAD, rsindex);
                        mv.visitInsn(LRETURN);
                    } else if (returnType == float.class) {
                        mv.visitVarInsn(FLOAD, rsindex);
                        mv.visitInsn(FRETURN);
                    } else if (returnType == double.class) {
                        mv.visitVarInsn(DLOAD, rsindex);
                        mv.visitInsn(DRETURN);
                    } else {
                        mv.visitVarInsn(ILOAD, rsindex);
                        mv.visitInsn(IRETURN);
                    }
                } else {
                    mv.visitVarInsn(ALOAD, rsindex);
                    mv.visitInsn(ARETURN);
                }
                mv.visitLabel(clientLabel);
                //---------------------------- 调用samerun ---------------------------------
                mv.visitVarInsn(ILOAD, 2); //读取 samerunnable
                Label sameLabel = new Label();
                mv.visitJumpInsn(IFEQ, sameLabel);  //判断 samerunnable

                mv.visitVarInsn(ALOAD, 0);//调用 _client
                mv.visitFieldInsn(GETFIELD, newDynName, FIELDPREFIX + "_client", clientDesc);
                final int preparams = 3; //调用selfrunnable之前的参数个数;  _client

                pushInt(mv, index);   //第几个 SncpAction 
                pushInt(mv, paramtypes.length + preparams);   //参数总数量

                mv.visitTypeInsn(ANEWARRAY, "java/lang/Object");

                mv.visitInsn(DUP);
                mv.visitInsn(ICONST_0);
                mv.visitInsn(ICONST_1);   //第一个参数  selfrunnable
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
                mv.visitInsn(AASTORE);

                mv.visitInsn(DUP);
                mv.visitInsn(ICONST_1);
                mv.visitInsn(ICONST_0);   //第二个参数  samerunnable
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
                mv.visitInsn(AASTORE);

                mv.visitInsn(DUP);
                mv.visitInsn(ICONST_2);
                mv.visitInsn(ICONST_0);   //第三个参数  diffrunnable
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
                mv.visitInsn(AASTORE);

                int insn = 3; //空3给selfrunnable、samerunnable、diffrunnable
                for (int j = 0; j < paramtypes.length; j++) {
                    final Class pt = paramtypes[j];
                    mv.visitInsn(DUP);
                    insn++;
                    pushInt(mv, j + 3);
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
                mv.visitMethodInsn(INVOKEVIRTUAL, clientName, mrun.async() ? "asyncRemoteSameGroup" : "remoteSameGroup", "(I[Ljava/lang/Object;)V", false);
                mv.visitLabel(sameLabel);
                //---------------------------- 调用diffrun ---------------------------------
                mv.visitVarInsn(ILOAD, 3); //读取 diffrunnable
                Label diffLabel = new Label();
                mv.visitJumpInsn(IFEQ, diffLabel);  //判断 diffrunnable

                mv.visitVarInsn(ALOAD, 0);
                mv.visitFieldInsn(GETFIELD, newDynName, FIELDPREFIX + "_client", clientDesc);

                pushInt(mv, index); //第几个 SncpAction 
                pushInt(mv, paramtypes.length + preparams); //参数总数量

                mv.visitTypeInsn(ANEWARRAY, "java/lang/Object");

                mv.visitInsn(DUP);
                mv.visitInsn(ICONST_0);
                mv.visitInsn(ICONST_1);   //第一个参数  samerunnable
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
                mv.visitInsn(AASTORE);

                mv.visitInsn(DUP);
                mv.visitInsn(ICONST_1);
                mv.visitInsn(ICONST_1);   //第二个参数  diffrunnable
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
                mv.visitInsn(AASTORE);

                mv.visitInsn(DUP);
                mv.visitInsn(ICONST_2);
                mv.visitInsn(ICONST_0);   //第二个参数  diffrunnable
                mv.visitMethodInsn(INVOKESTATIC, "java/lang/Boolean", "valueOf", "(Z)Ljava/lang/Boolean;", false);
                mv.visitInsn(AASTORE);

                insn = 3;//空3给selfrunnable、samerunnable、diffrunnable
                for (int j = 0; j < paramtypes.length; j++) {
                    final Class pt = paramtypes[j];
                    mv.visitInsn(DUP);
                    insn++;
                    pushInt(mv, j + 3);
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
                mv.visitMethodInsn(INVOKEVIRTUAL, clientName, mrun.async() ? "asyncRemoteDiffGroup" : "remoteDiffGroup", "(I[Ljava/lang/Object;)V", false);
                mv.visitLabel(diffLabel);

                if (returnType == void.class) {
                    mv.visitInsn(RETURN);
                } else if (returnType.isPrimitive()) {
                    if (returnType == long.class) {
                        mv.visitVarInsn(LLOAD, rsindex);
                        mv.visitInsn(LRETURN);
                    } else if (returnType == float.class) {
                        mv.visitVarInsn(FLOAD, rsindex);
                        mv.visitInsn(FRETURN);
                    } else if (returnType == double.class) {
                        mv.visitVarInsn(DLOAD, rsindex);
                        mv.visitInsn(DRETURN);
                    } else {
                        mv.visitVarInsn(ILOAD, rsindex);
                        mv.visitInsn(IRETURN);
                    }
                } else {
                    mv.visitVarInsn(ALOAD, rsindex);
                    mv.visitInsn(ARETURN);
                }

                mv.visitMaxs(Math.max(varindex, 10), varindex + 4);
                mv.visitEnd();
            }
        }
        cw.visitEnd();
        byte[] bytes = cw.toByteArray();
        Class<?> newClazz = new ClassLoader(loader) {
            public final Class<?> loadClass(String name, byte[] b) {
                return defineClass(name, b, 0, b.length);
            }
        }.loadClass(newDynName.replace('/', '.'), bytes);
        return (Class<T>) newClazz;
    }

    private static void visitAnnotation(final AnnotationVisitor av, final Annotation ann) {
        try {
            for (Method anm : ann.annotationType().getMethods()) {
                final String mname = anm.getName();
                if ("equals".equals(mname) || "hashCode".equals(mname) || "toString".equals(mname) || "annotationType".equals(mname)) continue;
                final Object r = anm.invoke(ann);
                if (r instanceof String[]) {
                    AnnotationVisitor av1 = av.visitArray(mname);
                    for (String item : (String[]) r) {
                        av1.visit(null, item);
                    }
                    av1.visitEnd();
                } else if (r instanceof Class[]) {
                    AnnotationVisitor av1 = av.visitArray(mname);
                    for (Class item : (Class[]) r) {
                        av1.visit(null, Type.getType(item));
                    }
                    av1.visitEnd();
                } else if (r instanceof Enum[]) {
                    AnnotationVisitor av1 = av.visitArray(mname);
                    for (Enum item : (Enum[]) r) {
                        av1.visitEnum(null, Type.getDescriptor(item.getClass()), ((Enum) item).name());
                    }
                    av1.visitEnd();
                } else if (r instanceof Annotation[]) {
                    AnnotationVisitor av1 = av.visitArray(mname);
                    for (Annotation item : (Annotation[]) r) {
                        visitAnnotation(av1.visitAnnotation(null, Type.getDescriptor(((Annotation) item).annotationType())), item);
                    }
                    av1.visitEnd();
                } else if (r instanceof Class) {
                    av.visit(mname, Type.getType((Class) r));
                } else if (r instanceof Enum) {
                    av.visitEnum(mname, Type.getDescriptor(r.getClass()), ((Enum) r).name());
                } else if (r instanceof Annotation) {
                    visitAnnotation(av.visitAnnotation(null, Type.getDescriptor(((Annotation) r).annotationType())), (Annotation) r);
                } else {
                    av.visit(mname, r);
                }
            }
            av.visitEnd();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static <T extends Service> T createSimpleLocalService(final Class<T> serviceImplClass,
        final TransportFactory transportFactory, final InetSocketAddress clientSncpAddress, final String... groups) {
        return createLocalService(null, "", serviceImplClass, ResourceFactory.root(), transportFactory, clientSncpAddress, Utility.ofSet(groups), null);
    }

    /**
     *
     * 创建本地模式Service实例
     *
     * @param <T>               Service泛型
     * @param classLoader       ClassLoader
     * @param name              资源名
     * @param serviceImplClass  Service类
     * @param resourceFactory   ResourceFactory
     * @param transportFactory  TransportFactory
     * @param clientSncpAddress 本地IP地址
     * @param groups            所有的组节点，包含自身
     * @param conf              启动配置项
     *
     * @return Service的本地模式实例
     */
    @SuppressWarnings("unchecked")
    public static <T extends Service> T createLocalService(
        final ClassLoader classLoader,
        final String name,
        final Class<T> serviceImplClass,
        final ResourceFactory resourceFactory,
        final TransportFactory transportFactory,
        final InetSocketAddress clientSncpAddress,
        final Set<String> groups,
        final AnyValue conf) {
        try {
            final Class newClazz = createLocalServiceClass(classLoader, name, serviceImplClass);
            T rs = (T) newClazz.getDeclaredConstructor().newInstance();
            //--------------------------------------            
            Service remoteService = null;
            {
                Class loop = newClazz;
                do {
                    for (Field field : loop.getDeclaredFields()) {
                        int mod = field.getModifiers();
                        if (Modifier.isFinal(mod) || Modifier.isStatic(mod)) continue;
                        if (field.getAnnotation(RpcRemote.class) == null) continue;
                        if (!field.getType().isAssignableFrom(newClazz)) continue;
                        field.setAccessible(true);
                        if (remoteService == null && clientSncpAddress != null) {
                            remoteService = createRemoteService(classLoader, name, serviceImplClass, transportFactory, clientSncpAddress, groups, conf);
                        }
                        if (remoteService != null) field.set(rs, remoteService);
                    }
                } while ((loop = loop.getSuperclass()) != Object.class);
            }
            SncpClient client = null;
            {
                try {
                    Field e = newClazz.getDeclaredField(FIELDPREFIX + "_client");
                    e.setAccessible(true);
                    client = new SncpClient(name, serviceImplClass, rs, transportFactory, false, newClazz, clientSncpAddress);
                    Set<String> diffGroups = groups == null ? new HashSet<>() : new HashSet<>(groups);
                    String sameGroup = transportFactory.findGroupName(clientSncpAddress);
                    if (sameGroup != null) diffGroups.remove(sameGroup);
                    client.setSameGroup(sameGroup);
                    client.setDiffGroups(diffGroups);
                    client.setSameGroupTransport(transportFactory.loadSameGroupTransport(clientSncpAddress));
                    client.setDiffGroupTransports(transportFactory.loadDiffGroupTransports(clientSncpAddress, diffGroups));
                    e.set(rs, client);
                    transportFactory.addSncpService(rs);
                } catch (NoSuchFieldException ne) {
                    ne.printStackTrace();
                }
            }
            if (client == null) return rs;
            {
                Field c = newClazz.getDeclaredField(FIELDPREFIX + "_conf");
                c.setAccessible(true);
                c.set(rs, conf);
            }
            return rs;
        } catch (RuntimeException rex) {
            throw rex;
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    public static <T extends Service> T createSimpleRemoteService(final Class<T> serviceImplClass,
        final TransportFactory transportFactory, final InetSocketAddress clientSncpAddress, final String... groups) {
        return createRemoteService(null, "", serviceImplClass, transportFactory, clientSncpAddress, Utility.ofSet(groups), null);
    }

    /**
     * <blockquote><pre>
     * &#64;Resource(name = "")
     * &#64;SncpDyn(remote = true)
     * &#64;ResourceType(TestService.class)
     * public final class _DynRemoteTestService extends TestService{
     *
     *      private AnyValue _redkale_conf;
     *
     *      private SncpClient _redkale_client;
     *
     *      &#64;SncpDyn(remote = false, index = 0)
     *      public void _redkale_createSomeThing(boolean selfrunnable, boolean samerunnable, boolean diffrunnable, TestBean bean){
     *          _redkale_client.remote(0, selfrunnable, samerunnable, diffrunnable, bean);
     *      }
     *
     *      &#64;SncpDyn(remote = false, index = 1)
     *      public String _redkale_updateSomeThing(boolean selfrunnable, boolean samerunnable, boolean diffrunnable, String id){
     *          return _redkale_client.remote(1, selfrunnable, samerunnable, diffrunnable, id);
     *      }
     *
     *      &#64;Override
     *      public void createSomeThing(TestBean bean){
     *          _redkale_client.remote(2, bean);
     *      }
     *
     *      &#64;Override
     *      public String findSomeThing(){
     *          return _redkale_client.remote(3);
     *      }
     *
     *      &#64;Override
     *      public String updateSomeThing(String id){
     *          return  _redkale_client.remote(4, id);
     *      }
     * }
     * </pre></blockquote>
     *
     * 创建远程模式的Service实例
     *
     * @param <T>                    Service泛型
     * @param classLoader            ClassLoader
     * @param name                   资源名
     * @param serviceTypeOrImplClass Service类
     * @param transportFactory       TransportFactory
     * @param clientAddress          本地IP地址
     * @param groups0                 所有的组节点，包含自身
     * @param conf                   启动配置项
     *
     * @return Service的远程模式实例
     */
    @SuppressWarnings("unchecked")

    public static <T extends Service> T createRemoteService(
        final ClassLoader classLoader,
        final String name,
        final Class<T> serviceTypeOrImplClass,
        final TransportFactory transportFactory,
        final InetSocketAddress clientAddress,
        final Set<String> groups0,
        final AnyValue conf) {
        if (serviceTypeOrImplClass == null) return null;
        if (!Service.class.isAssignableFrom(serviceTypeOrImplClass)) return null;
        Set<String> groups = groups0 == null ? new HashSet<>() : groups0;
        ResourceFactory.checkResourceName(name);
        int mod = serviceTypeOrImplClass.getModifiers();
        boolean realed = !(java.lang.reflect.Modifier.isAbstract(mod) || serviceTypeOrImplClass.isInterface());
        if (!java.lang.reflect.Modifier.isPublic(mod)) return null;
        final String supDynName = serviceTypeOrImplClass.getName().replace('.', '/');
        final String clientName = SncpClient.class.getName().replace('.', '/');
        final String resDesc = Type.getDescriptor(Resource.class);
        final String clientDesc = Type.getDescriptor(SncpClient.class);
        final String sncpDynDesc = Type.getDescriptor(SncpDyn.class);
        final String anyValueDesc = Type.getDescriptor(AnyValue.class);
        ClassLoader loader = classLoader == null ? Thread.currentThread().getContextClassLoader() : classLoader;
        String newDynName = supDynName.substring(0, supDynName.lastIndexOf('/') + 1) + REMOTEPREFIX + serviceTypeOrImplClass.getSimpleName();
        try {
            Class newClazz = loader.loadClass(newDynName.replace('/', '.'));
            T rs = (T) newClazz.getDeclaredConstructor().newInstance();
            SncpClient client = new SncpClient(name, serviceTypeOrImplClass, rs, transportFactory, true, realed ? createLocalServiceClass(loader, name, serviceTypeOrImplClass) : serviceTypeOrImplClass, clientAddress);
            client.setRemoteGroups(groups);
            client.setRemoteGroupTransport(transportFactory.loadRemoteTransport(clientAddress, groups));
            Field c = newClazz.getDeclaredField(FIELDPREFIX + "_client");
            c.setAccessible(true);
            c.set(rs, client);
            transportFactory.addSncpService(rs);
            return rs;
        } catch (Throwable ex) {
        }
        //------------------------------------------------------------------------------
        ClassWriter cw = new ClassWriter(COMPUTE_FRAMES);
        FieldVisitor fv;
        MethodDebugVisitor mv;
        AnnotationVisitor av0;

        cw.visit(V1_8, ACC_PUBLIC + ACC_FINAL + ACC_SUPER, newDynName, null, serviceTypeOrImplClass.isInterface() ? "java/lang/Object" : supDynName, serviceTypeOrImplClass.isInterface() ? new String[]{supDynName} : null);
        {
            av0 = cw.visitAnnotation(resDesc, true);
            av0.visit("name", name);
            av0.visitEnd();
        }
        {
            av0 = cw.visitAnnotation(Type.getDescriptor(ResourceType.class), true);
            ResourceType rty = serviceTypeOrImplClass.getAnnotation(ResourceType.class);
            av0.visit("value", Type.getType(Type.getDescriptor(rty == null ? serviceTypeOrImplClass : rty.value())));
            av0.visitEnd();
        }
        {
            av0 = cw.visitAnnotation(sncpDynDesc, true);
            av0.visit("remote", Boolean.TRUE);
            av0.visitEnd();
        }
        { //给新类加上 原有的Annotation
            for (Annotation ann : serviceTypeOrImplClass.getAnnotations()) {
                if (ann instanceof Resource || ann instanceof SncpDyn || ann instanceof ResourceType) continue;
                visitAnnotation(cw.visitAnnotation(Type.getDescriptor(ann.annotationType()), true), ann);
            }
        }
        {
            fv = cw.visitField(ACC_PRIVATE, FIELDPREFIX + "_conf", anyValueDesc, null, null);
            fv.visitEnd();
        }
        {
            fv = cw.visitField(ACC_PRIVATE, FIELDPREFIX + "_client", clientDesc, null, null);
            fv.visitEnd();
        }
        { //构造函数
            mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null));
            //mv.setDebug(true);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, serviceTypeOrImplClass.isInterface() ? "java/lang/Object" : supDynName, "<init>", "()V", false);
            mv.visitInsn(RETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }
        { //init
            mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "init", "(" + anyValueDesc + ")V", null, null));
            mv.visitInsn(RETURN);
            mv.visitMaxs(0, 2);
            mv.visitEnd();
        }
        { //destroy
            mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "destroy", "(" + anyValueDesc + ")V", null, null));
            mv.visitInsn(RETURN);
            mv.visitMaxs(0, 2);
            mv.visitEnd();
        }
        { // toString()
            mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "toString", "()Ljava/lang/String;", null, null));
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, newDynName, FIELDPREFIX + "_client", clientDesc);
            Label l1 = new Label();
            mv.visitJumpInsn(IFNONNULL, l1);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "getClass", "()Ljava/lang/Class;", false);
            mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Class", "getName", "()Ljava/lang/String;", false);
            Label l2 = new Label();
            mv.visitJumpInsn(GOTO, l2);
            mv.visitLabel(l1);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitFieldInsn(GETFIELD, newDynName, FIELDPREFIX + "_client", clientDesc);
            mv.visitMethodInsn(INVOKEVIRTUAL, clientName, "toSimpleString", "()Ljava/lang/String;", false);
            mv.visitLabel(l2);
            mv.visitInsn(ARETURN);
            mv.visitMaxs(1, 1);
            mv.visitEnd();
        }
        int i = -1;
        for (final SncpAction entry : SncpClient.getSncpActions(realed ? createLocalServiceClass(loader, name, serviceTypeOrImplClass) : serviceTypeOrImplClass)) {
            final int index = ++i;
            final java.lang.reflect.Method method = entry.method;
            {
                mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, method.getName(), Type.getMethodDescriptor(method), null, null));
                //mv.setDebug(true);
                { //给参数加上 Annotation
                    final Annotation[][] anns = method.getParameterAnnotations();
                    for (int k = 0; k < anns.length; k++) {
                        for (Annotation ann : anns[k]) {
                            visitAnnotation(mv.visitParameterAnnotation(k, Type.getDescriptor(ann.annotationType()), true), ann);
                        }
                    }
                }
                mv.visitVarInsn(ALOAD, 0);
                mv.visitFieldInsn(GETFIELD, newDynName, FIELDPREFIX + "_client", clientDesc);

                pushInt(mv, index);

                {  //传参数
                    int paramlen = entry.paramTypes.length;
                    pushInt(mv, paramlen);
                    mv.visitTypeInsn(ANEWARRAY, "java/lang/Object");
                    java.lang.reflect.Type[] paramtypes = entry.paramTypes;
                    int insn = 0;
                    for (int j = 0; j < paramtypes.length; j++) {
                        final java.lang.reflect.Type pt = paramtypes[j];
                        mv.visitInsn(DUP);
                        insn++;
                        pushInt(mv, j);
                        if (pt instanceof Class && ((Class) pt).isPrimitive()) {
                            if (pt == long.class) {
                                mv.visitVarInsn(LLOAD, insn++);
                            } else if (pt == float.class) {
                                mv.visitVarInsn(FLOAD, insn++);
                            } else if (pt == double.class) {
                                mv.visitVarInsn(DLOAD, insn++);
                            } else {
                                mv.visitVarInsn(ILOAD, insn);
                            }
                            Class bigclaz = java.lang.reflect.Array.get(java.lang.reflect.Array.newInstance((Class) pt, 1), 0).getClass();
                            mv.visitMethodInsn(INVOKESTATIC, bigclaz.getName().replace('.', '/'), "valueOf", "(" + Type.getDescriptor((Class) pt) + ")" + Type.getDescriptor(bigclaz), false);
                        } else {
                            mv.visitVarInsn(ALOAD, insn);
                        }
                        mv.visitInsn(AASTORE);
                    }
                }

                mv.visitMethodInsn(INVOKEVIRTUAL, clientName, "remote", "(I[Ljava/lang/Object;)Ljava/lang/Object;", false);
                //mv.visitMethodInsn(INVOKEVIRTUAL, convertName, "convertFrom", convertFromDesc, false);
                if (method.getGenericReturnType() == void.class) {
                    mv.visitInsn(POP);
                    mv.visitInsn(RETURN);
                } else {
                    Class returnclz = method.getReturnType();
                    Class bigPrimitiveClass = returnclz.isPrimitive() ? java.lang.reflect.Array.get(java.lang.reflect.Array.newInstance(returnclz, 1), 0).getClass() : returnclz;
                    mv.visitTypeInsn(CHECKCAST, (returnclz.isPrimitive() ? bigPrimitiveClass : returnclz).getName().replace('.', '/'));
                    if (returnclz.isPrimitive()) {
                        String bigPrimitiveName = bigPrimitiveClass.getName().replace('.', '/');
                        try {
                            java.lang.reflect.Method pm = bigPrimitiveClass.getMethod(returnclz.getSimpleName() + "Value");
                            mv.visitMethodInsn(INVOKEVIRTUAL, bigPrimitiveName, pm.getName(), Type.getMethodDescriptor(pm), false);
                        } catch (Exception ex) {
                            throw new RuntimeException(ex); //不可能会发生
                        }
                        if (returnclz == long.class) {
                            mv.visitInsn(LRETURN);
                        } else if (returnclz == float.class) {
                            mv.visitInsn(FRETURN);
                        } else if (returnclz == double.class) {
                            mv.visitInsn(DRETURN);
                        } else {
                            mv.visitInsn(IRETURN);
                        }
                    } else {
                        mv.visitInsn(ARETURN);
                    }
                }
                mv.visitMaxs(20, 20);
                mv.visitEnd();
            }
        }
        cw.visitEnd();
        byte[] bytes = cw.toByteArray();
        Class<?> newClazz = new ClassLoader(loader) {
            public final Class<?> loadClass(String name, byte[] b) {
                return defineClass(name, b, 0, b.length);
            }
        }.loadClass(newDynName.replace('/', '.'), bytes);
        try {
            T rs = (T) newClazz.getDeclaredConstructor().newInstance();
            SncpClient client = new SncpClient(name, serviceTypeOrImplClass, rs, transportFactory, true, realed ? createLocalServiceClass(loader, name, serviceTypeOrImplClass) : serviceTypeOrImplClass, clientAddress);
            client.setRemoteGroups(groups);
            client.setRemoteGroupTransport(transportFactory.loadRemoteTransport(clientAddress, groups));
            {
                Field c = newClazz.getDeclaredField(FIELDPREFIX + "_client");
                c.setAccessible(true);
                c.set(rs, client);
            }
            {
                Field c = newClazz.getDeclaredField(FIELDPREFIX + "_conf");
                c.setAccessible(true);
                c.set(rs, conf);
            }
            transportFactory.addSncpService(rs);
            return rs;
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }

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
}
