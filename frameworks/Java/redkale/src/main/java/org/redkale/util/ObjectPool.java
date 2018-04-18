/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.util;

import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.util.function.*;
import java.util.logging.*;

/**
 * 对象池
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> 对象池元素的数据类型
 */
public final class ObjectPool<T> implements Supplier<T>, Consumer<T> {

    private static final Logger logger = Logger.getLogger(ObjectPool.class.getSimpleName());

    private final boolean debug;

    private final Queue<T> queue;

    private Creator<T> creator;

    private final Consumer<T> prepare;

    private final Predicate<T> recycler;

    private final AtomicLong creatCounter;

    private final AtomicLong cycleCounter;

    public ObjectPool(Class<T> clazz, Consumer<T> prepare, Predicate<T> recycler) {
        this(2, clazz, prepare, recycler);
    }

    public ObjectPool(int max, Class<T> clazz, Consumer<T> prepare, Predicate<T> recycler) {
        this(max, Creator.create(clazz), prepare, recycler);
    }

    public ObjectPool(Creator<T> creator, Consumer<T> prepare, Predicate<T> recycler) {
        this(2, creator, prepare, recycler);
    }

    public ObjectPool(int max, Creator<T> creator, Consumer<T> prepare, Predicate<T> recycler) {
        this(null, null, max, creator, prepare, recycler);
    }

    public ObjectPool(AtomicLong creatCounter, AtomicLong cycleCounter, int max, Creator<T> creator, Consumer<T> prepare, Predicate<T> recycler) {
        this.creatCounter = creatCounter;
        this.cycleCounter = cycleCounter;
        this.creator = creator;
        this.prepare = prepare;
        this.recycler = recycler;
        this.queue = new LinkedBlockingQueue<>(Math.max(Runtime.getRuntime().availableProcessors() * 2, max));
        this.debug = logger.isLoggable(Level.FINEST);
    }

    public void setCreator(Creator<T> creator) {
        this.creator = creator;
    }

    public Predicate<T> getRecyclerPredicate() {
        return recycler;
    }

    @Override
    public T get() {
        T result = queue.poll();
        if (result == null) {
            if (creatCounter != null) creatCounter.incrementAndGet();
            result = this.creator.create();
        }
        if (prepare != null) prepare.accept(result);
        return result;
    }

    @Override
    public void accept(final T e) {
        if (e != null && recycler.test(e)) {
            if (cycleCounter != null) cycleCounter.incrementAndGet();
//            if (debug) {
//                for (T t : queue) {
//                    if (t == e) {
//                        logger.log(Level.WARNING, "[" + Thread.currentThread().getName() + "] repeat offer the same object(" + e + ")", new Exception());
//                        return;
//                    }
//                }
//            }
            queue.offer(e);
        }
    }

    public long getCreatCount() {
        return creatCounter.longValue();
    }

    public long getCycleCount() {
        return cycleCounter.longValue();
    }
}
