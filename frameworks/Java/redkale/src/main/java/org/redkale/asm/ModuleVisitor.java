/*
 * ORACLE PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

/*
 *
 *
 *
 *
 *
 * ASM: a very small and fast Java bytecode manipulation framework
 * Copyright (c) 2000-2011 INRIA, France Telecom
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.redkale.asm;

/**
 * A visitor to visit a Java module. The methods of this class must be called in
 * the following order: <tt>visitMainClass</tt> | ( <tt>visitPackage</tt> |
 * <tt>visitRequire</tt> | <tt>visitExport</tt> | <tt>visitOpen</tt> |
 * <tt>visitUse</tt> | <tt>visitProvide</tt> )* <tt>visitEnd</tt>.
 *
 * The methods {@link #visitRequire(String, int, String)}, {@link #visitExport(String, int, String...)},
 * {@link #visitOpen(String, int, String...)} and {@link #visitPackage(String)}
 * take as parameter a package name or a module name. Unlike the other names which are internal names
 * (names separated by slash), module and package names are qualified names (names separated by dot).
 *
 * @author Remi Forax
 */
public abstract class ModuleVisitor {
    /**
     * The ASM API version implemented by this visitor. The value of this field
     * must be {@link Opcodes#ASM6}.
     */
    protected final int api;

    /**
     * The module visitor to which this visitor must delegate method calls. May
     * be null.
     */
    protected ModuleVisitor mv;

    /**
     * Constructs a new {@link ModuleVisitor}.
     *
     * @param api
     *            the ASM API version implemented by this visitor. Must be {@link Opcodes#ASM6}.
     */
    public ModuleVisitor(final int api) {
        this(api, null);
    }

    /**
     * Constructs a new {@link ModuleVisitor}.
     *
     * @param api
     *            the ASM API version implemented by this visitor. Must be {@link Opcodes#ASM6}.
     * @param mv
     *            the module visitor to which this visitor must delegate method
     *            calls. May be null.
     */
    public ModuleVisitor(final int api, final ModuleVisitor mv) {
        if (api != Opcodes.ASM6) {
            throw new IllegalArgumentException();
        }
        this.api = api;
        this.mv = mv;
    }

    /**
     * Visit the main class of the current module.
     *
     * @param mainClass the internal name of the main class of the current module.
     */
    public void visitMainClass(String mainClass) {
        if (mv != null) {
            mv.visitMainClass(mainClass);
        }
    }

    /**
     * Visit a package of the current module.
     *
     * @param packaze the qualified name of a package.
     */
    public void visitPackage(String packaze) {
        if (mv != null) {
            mv.visitPackage(packaze);
        }
    }

    /**
     * Visits a dependence of the current module.
     *
     * @param module the qualified name of the dependence.
     * @param access the access flag of the dependence among
     *        ACC_TRANSITIVE, ACC_STATIC_PHASE, ACC_SYNTHETIC
     *        and ACC_MANDATED.
     * @param version the module version at compile time or null.
     */
    public void visitRequire(String module, int access, String version) {
        if (mv != null) {
            mv.visitRequire(module, access, version);
        }
    }

    /**
     * Visit an exported package of the current module.
     *
     * @param packaze the qualified name of the exported package.
     * @param access the access flag of the exported package,
     *        valid values are among {@code ACC_SYNTHETIC} and
     *        {@code ACC_MANDATED}.
     * @param modules the qualified names of the modules that can access to
     *        the public classes of the exported package or
     *        <tt>null</tt>.
     */
    public void visitExport(String packaze, int access, String... modules) {
        if (mv != null) {
            mv.visitExport(packaze, access, modules);
        }
    }

    /**
     * Visit an open package of the current module.
     *
     * @param packaze the qualified name of the opened package.
     * @param access the access flag of the opened package,
     *        valid values are among {@code ACC_SYNTHETIC} and
     *        {@code ACC_MANDATED}.
     * @param modules the qualified names of the modules that can use deep
     *        reflection to the classes of the open package or
     *        <tt>null</tt>.
     */
    public void visitOpen(String packaze, int access, String... modules) {
        if (mv != null) {
            mv.visitOpen(packaze, access, modules);
        }
    }

    /**
     * Visit a service used by the current module.
     * The name must be the internal name of an interface or a class.
     *
     * @param service the internal name of the service.
     */
    public void visitUse(String service) {
        if (mv != null) {
            mv.visitUse(service);
        }
    }

    /**
     * Visit an implementation of a service.
     *
     * @param service the internal name of the service
     * @param providers the internal names of the implementations
     *        of the service (there is at least one provider).
     */
    public void visitProvide(String service, String... providers) {
        if (mv != null) {
            mv.visitProvide(service, providers);
        }
    }

    /**
     * Visits the end of the module. This method, which is the last one to be
     * called, is used to inform the visitor that everything have been visited.
     */
    public void visitEnd() {
        if (mv != null) {
            mv.visitEnd();
        }
    }
}
