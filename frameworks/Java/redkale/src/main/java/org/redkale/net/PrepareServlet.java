/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.net;

import java.io.*;
import java.nio.*;
import java.nio.channels.*;
import java.util.*;
import java.util.concurrent.atomic.*;
import java.util.function.Predicate;
import java.util.logging.*;
import org.redkale.util.*;

/**
 * 根Servlet， 一个Server只能存在一个根Servlet
 *
 * 用于分发Request请求
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <K> SessionID的类型
 * @param <C> Context的子类型
 * @param <R> Request的子类型
 * @param <P> Response的子类型
 * @param <S> Servlet的子类型
 */
public abstract class PrepareServlet<K extends Serializable, C extends Context, R extends Request<C>, P extends Response<C, R>, S extends Servlet<C, R, P>> extends Servlet<C, R, P> {

    protected final AtomicLong executeCounter = new AtomicLong(); //执行请求次数

    protected final AtomicLong illRequestCounter = new AtomicLong(); //错误请求次数

    private final Object lock1 = new Object();

    private Set<S> servlets = new HashSet<>();

    private final Object lock2 = new Object();

    private Map<K, S> mappings = new HashMap<>();

    private final List<Filter<C, R, P>> filters = new ArrayList<>();

    protected Filter<C, R, P> headFilter;

    protected void putServlet(S servlet) {
        synchronized (lock1) {
            Set<S> newservlets = new HashSet<>(servlets);
            newservlets.add(servlet);
            this.servlets = newservlets;
        }
    }

    protected void removeServlet(S servlet) {
        synchronized (lock1) {
            Set<S> newservlets = new HashSet<>(servlets);
            newservlets.remove(servlet);
            this.servlets = newservlets;
        }
    }

    public boolean containsServlet(Class<? extends S> servletClass) {
        synchronized (lock1) {
            for (S servlet : new HashSet<>(servlets)) {
                if (servlet.getClass().equals(servletClass)) return true;
            }
            return false;
        }
    }

    public boolean containsServlet(String servletClassName) {
        synchronized (lock1) {
            for (S servlet : new HashSet<>(servlets)) {
                if (servlet.getClass().getName().equals(servletClassName)) return true;
            }
            return false;
        }
    }

    protected void putMapping(K key, S servlet) {
        synchronized (lock2) {
            Map<K, S> newmappings = new HashMap<>(mappings);
            newmappings.put(key, servlet);
            this.mappings = newmappings;
        }
    }

    protected void removeMapping(K key) {
        synchronized (lock2) {
            if (mappings.containsKey(key)) {
                Map<K, S> newmappings = new HashMap<>(mappings);
                newmappings.remove(key);
                this.mappings = newmappings;
            }
        }
    }

    protected void removeMapping(S servlet) {
        synchronized (lock2) {
            List<K> keys = new ArrayList<>();
            Map<K, S> newmappings = new HashMap<>(mappings);
            for (Map.Entry<K, S> en : newmappings.entrySet()) {
                if (en.getValue().equals(servlet)) {
                    keys.add(en.getKey());
                }
            }
            for (K key : keys) newmappings.remove(key);
            this.mappings = newmappings;
        }
    }

    protected S mappingServlet(K key) {
        return mappings.get(key);
    }

    @Override
    @SuppressWarnings("unchecked")
    public void init(C context, AnyValue config) {
        synchronized (filters) {
            if (!filters.isEmpty()) {
                Collections.sort(filters);
                for (Filter<C, R, P> filter : filters) {
                    filter.init(context, config);
                }
                this.headFilter = filters.get(0);
                Filter<C, R, P> filter = this.headFilter;
                for (int i = 1; i < filters.size(); i++) {
                    filter._next = filters.get(i);
                    filter = filter._next;
                }
            }
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public void destroy(C context, AnyValue config) {
        synchronized (filters) {
            if (!filters.isEmpty()) {
                for (Filter filter : filters) {
                    filter.destroy(context, config);
                }
            }
        }
    }

    @SuppressWarnings("unchecked")
    public void addFilter(Filter<C, R, P> filter, AnyValue conf) {
        filter._conf = conf;
        synchronized (filters) {
            this.filters.add(filter);
            Collections.sort(this.filters);
        }
    }

    public <T extends Filter<C, R, P>> T removeFilter(Class<T> filterClass) {
        return removeFilter(f -> filterClass.equals(f.getClass()));
    }

    public boolean containsFilter(Class<? extends Filter> filterClass) {
        if (this.headFilter == null || filterClass == null) return false;
        Filter filter = this.headFilter;
        do {
            if (filter.getClass().equals(filterClass)) return true;
        } while ((filter = filter._next) != null);
        return false;
    }

    public boolean containsFilter(String filterClassName) {
        if (this.headFilter == null || filterClassName == null) return false;
        Filter filter = this.headFilter;
        do {
            if (filter.getClass().getName().equals(filterClassName)) return true;
        } while ((filter = filter._next) != null);
        return false;
    }

    @SuppressWarnings("unchecked")
    public <T extends Filter<C, R, P>> T removeFilter(Predicate<T> predicate) {
        if (this.headFilter == null || predicate == null) return null;
        synchronized (filters) {
            Filter filter = this.headFilter;
            Filter prev = null;
            do {
                if (predicate.test((T) filter)) break;
                prev = filter;
            } while ((filter = filter._next) != null);
            if (filter != null) {
                if (prev == null) {
                    this.headFilter = filter._next;
                } else {
                    prev._next = filter._next;
                }
                filter._next = null;
                this.filters.remove(filter);
            }
            return (T) filter;
        }
    }

    @SuppressWarnings("unchecked")
    public <T extends Filter<C, R, P>> List<T> getFilters() {
        return (List) new ArrayList<>(filters);
    }

    @SuppressWarnings("unchecked")
    public abstract void addServlet(S servlet, Object attachment, AnyValue conf, K... mappings);

    public final void prepare(final ByteBuffer buffer, final R request, final P response) throws IOException {
        executeCounter.incrementAndGet();
        final int rs = request.readHeader(buffer);
        if (rs < 0) {
            request.offerReadBuffer(buffer);
            if (rs != Integer.MIN_VALUE) illRequestCounter.incrementAndGet();
            response.finish(true);
        } else if (rs == 0) {
            request.offerReadBuffer(buffer);
            request.prepare();
            response.filter = this.headFilter;
            response.servlet = this;
            response.nextEvent();
        } else {
            buffer.clear();
            final AtomicInteger ai = new AtomicInteger(rs);
            request.channel.read(buffer, buffer, new CompletionHandler<Integer, ByteBuffer>() {

                @Override
                public void completed(Integer result, ByteBuffer attachment) {
                    buffer.flip();
                    ai.addAndGet(-request.readBody(buffer));
                    if (ai.get() > 0) {
                        buffer.clear();
                        request.channel.read(buffer, buffer, this);
                    } else {
                        request.offerReadBuffer(buffer);
                        request.prepare();
                        try {
                            response.filter = PrepareServlet.this.headFilter;
                            response.servlet = PrepareServlet.this;
                            response.nextEvent();
                        } catch (Exception e) {
                            illRequestCounter.incrementAndGet();
                            response.finish(true);
                            request.context.logger.log(Level.WARNING, "prepare servlet abort, forece to close channel ", e);
                        }
                    }
                }

                @Override
                public void failed(Throwable exc, ByteBuffer attachment) {
                    illRequestCounter.incrementAndGet();
                    request.offerReadBuffer(buffer);
                    response.finish(true);
                    if (exc != null) request.context.logger.log(Level.FINER, "Servlet read channel erroneous, forece to close channel ", exc);
                }
            });
        }
    }

    protected AnyValue getServletConf(Servlet servlet) {
        return servlet._conf;
    }

    protected void setServletConf(Servlet servlet, AnyValue conf) {
        servlet._conf = conf;
    }

    public List<S> getServlets() {
        return new ArrayList<>(servlets);
    }
}
