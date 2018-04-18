/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net.http;

import org.redkale.asm.MethodDebugVisitor;
import java.net.*;
import java.nio.*;
import java.nio.channels.CompletionHandler;
import java.nio.charset.*;
import java.security.*;
import java.util.concurrent.*;
import java.util.logging.*;
import javax.net.ssl.SSLContext;
import org.redkale.asm.*;
import static org.redkale.asm.Opcodes.*;
import org.redkale.net.*;
import org.redkale.util.*;

/**
 * HTTP服务的上下文对象
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class HttpContext extends Context {

    protected final SecureRandom random = new SecureRandom();

    protected final ConcurrentHashMap<Class, Creator> asyncHandlerCreators = new ConcurrentHashMap<>();

    public HttpContext(long serverStartTime, Logger logger, ThreadPoolExecutor executor, SSLContext sslContext,
        final int bufferCapacity, final ObjectPool<ByteBuffer> bufferPool, ObjectPool<Response> responsePool,
        int maxbody, Charset charset, InetSocketAddress address, ResourceFactory resourceFactory,
        PrepareServlet prepare, int aliveTimeoutSeconds, int readTimeoutSeconds, int writeTimeoutSeconds) {
        super(serverStartTime, logger, executor, sslContext, bufferCapacity, bufferPool, responsePool,
            maxbody, charset, address, resourceFactory, prepare, aliveTimeoutSeconds, readTimeoutSeconds, writeTimeoutSeconds);

        random.setSeed(Math.abs(System.nanoTime()));
    }

    protected String createSessionid() {
        byte[] bytes = new byte[16];
        random.nextBytes(bytes);
        return new String(Utility.binToHex(bytes));
    }

    protected ExecutorService getExecutor() {
        return executor;
    }

    protected ObjectPool<Response> getResponsePool() {
        return responsePool;
    }

    @SuppressWarnings("unchecked")
    protected <H extends CompletionHandler> Creator<H> loadAsyncHandlerCreator(Class<H> handlerClass) {
        Creator<H> creator = asyncHandlerCreators.get(handlerClass);
        if (creator == null) {
            creator = createAsyncHandlerCreator(handlerClass);
            asyncHandlerCreators.put(handlerClass, creator);
        }
        return creator;
    }

    @SuppressWarnings("unchecked")
    private <H extends CompletionHandler> Creator<H> createAsyncHandlerCreator(Class<H> handlerClass) {
        //生成规则与SncpAsyncHandler.Factory 很类似
        //------------------------------------------------------------- 
        final boolean handlerinterface = handlerClass.isInterface();
        final String cpDesc = Type.getDescriptor(ConstructorParameters.class);
        final String handlerClassName = handlerClass.getName().replace('.', '/');
        final String handlerName = CompletionHandler.class.getName().replace('.', '/');
        final String handlerDesc = Type.getDescriptor(CompletionHandler.class);
        final String newDynName = handlerClass.getName().replace('.', '/') + "_DyncAsyncHandler_" + (System.currentTimeMillis() % 10000);

        ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
        FieldVisitor fv;
        MethodDebugVisitor mv;
        AnnotationVisitor av0;
        cw.visit(V1_8, ACC_PUBLIC + ACC_SUPER, newDynName, null, handlerinterface ? "java/lang/Object" : handlerClassName, handlerinterface ? new String[]{handlerClassName} : new String[]{handlerName});

        { //handler 属性
            fv = cw.visitField(ACC_PRIVATE, "handler", handlerDesc, null, null);
            fv.visitEnd();
        }
        {//构造方法
            mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "<init>", "(" + handlerDesc + ")V", null, null));
            //mv.setDebug(true);
            {
                av0 = mv.visitAnnotation(cpDesc, true);
                {
                    AnnotationVisitor av1 = av0.visitArray("value");
                    av1.visit(null, "handler");
                    av1.visitEnd();
                }
                av0.visitEnd();
            }
            mv.visitVarInsn(ALOAD, 0);
            mv.visitMethodInsn(INVOKESPECIAL, handlerinterface ? "java/lang/Object" : handlerClassName, "<init>", "()V", false);
            mv.visitVarInsn(ALOAD, 0);
            mv.visitVarInsn(ALOAD, 1);
            mv.visitFieldInsn(PUTFIELD, newDynName, "handler", handlerDesc);
            mv.visitInsn(RETURN);
            mv.visitMaxs(2, 2);
            mv.visitEnd();
        }

        for (java.lang.reflect.Method method : handlerClass.getMethods()) { //
            if ("completed".equals(method.getName()) && method.getParameterCount() == 2) {
                mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "completed", Type.getMethodDescriptor(method), null, null));
                mv.visitVarInsn(ALOAD, 0);
                mv.visitFieldInsn(GETFIELD, newDynName, "handler", handlerDesc);
                mv.visitVarInsn(ALOAD, 1);
                mv.visitVarInsn(ALOAD, 2);
                mv.visitMethodInsn(INVOKEINTERFACE, handlerName, "completed", "(Ljava/lang/Object;Ljava/lang/Object;)V", true);
                mv.visitInsn(RETURN);
                mv.visitMaxs(3, 3);
                mv.visitEnd();
            } else if ("failed".equals(method.getName()) && method.getParameterCount() == 2) {
                mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, "failed", Type.getMethodDescriptor(method), null, null));
                mv.visitVarInsn(ALOAD, 0);
                mv.visitFieldInsn(GETFIELD, newDynName, "handler", handlerDesc);
                mv.visitVarInsn(ALOAD, 1);
                mv.visitVarInsn(ALOAD, 2);
                mv.visitMethodInsn(INVOKEINTERFACE, handlerName, "failed", "(Ljava/lang/Throwable;Ljava/lang/Object;)V", true);
                mv.visitInsn(RETURN);
                mv.visitMaxs(3, 3);
                mv.visitEnd();
            } else if (handlerinterface || java.lang.reflect.Modifier.isAbstract(method.getModifiers())) {
                mv = new MethodDebugVisitor(cw.visitMethod(ACC_PUBLIC, method.getName(), Type.getMethodDescriptor(method), null, null));
                Class returnType = method.getReturnType();
                if (returnType == void.class) {
                    mv.visitInsn(RETURN);
                    mv.visitMaxs(0, 1);
                } else if (returnType.isPrimitive()) {
                    mv.visitInsn(ICONST_0);
                    if (returnType == long.class) {
                        mv.visitInsn(LRETURN);
                        mv.visitMaxs(2, 1);
                    } else if (returnType == float.class) {
                        mv.visitInsn(FRETURN);
                        mv.visitMaxs(2, 1);
                    } else if (returnType == double.class) {
                        mv.visitInsn(DRETURN);
                        mv.visitMaxs(2, 1);
                    } else {
                        mv.visitInsn(IRETURN);
                        mv.visitMaxs(1, 1);
                    }
                } else {
                    mv.visitInsn(ACONST_NULL);
                    mv.visitInsn(ARETURN);
                    mv.visitMaxs(1, 1);
                }
                mv.visitEnd();
            }
        }
        cw.visitEnd();
        byte[] bytes = cw.toByteArray();
        Class<CompletionHandler> newHandlerClazz = (Class<CompletionHandler>) new ClassLoader(handlerClass.getClassLoader()) {
            public final Class<?> loadClass(String name, byte[] b) {
                return defineClass(name, b, 0, b.length);
            }
        }.loadClass(newDynName.replace('/', '.'), bytes);
        return (Creator<H>) Creator.create(newHandlerClazz);
    }
}
