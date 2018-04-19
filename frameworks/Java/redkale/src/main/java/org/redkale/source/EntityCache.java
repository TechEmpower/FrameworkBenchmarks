/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.util.function.*;
import java.util.logging.*;
import java.util.stream.*;
import javax.persistence.*;
import static org.redkale.source.FilterFunc.*;
import org.redkale.util.*;

/**
 * Entity数据的缓存类
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> Entity类的泛型
 */
@SuppressWarnings("unchecked")
public final class EntityCache<T> {

    //日志
    private static final Logger logger = Logger.getLogger(EntityCache.class.getName());

    //主键与对象的键值对
    private ConcurrentHashMap<Serializable, T> map = new ConcurrentHashMap();

    // CopyOnWriteArrayList 插入慢、查询快; 10w数据插入需要3.2秒; ConcurrentLinkedQueue 插入快、查询慢；10w数据查询需要 0.062秒，  查询慢40%;
    private Collection<T> list = new ConcurrentLinkedQueue();

    //Flipper.sort转换成Comparator的缓存
    private final Map<String, Comparator<T>> sortComparators = new ConcurrentHashMap<>();

    //Entity类
    private final Class<T> type;

    //接口返回的对象是否需要复制一份
    private final boolean needcopy;

    //Entity构建器
    private final Creator<T> creator;

    //主键字段
    private final Attribute<T, Serializable> primary;

    //新增时的复制器， 排除了标记为&#064;Transient的字段
    private final Reproduce<T, T> newReproduce;

    //修改时的复制器， 排除了标记为&#064;Transient或&#064;Column(updatable=false)的字段
    private final Reproduce<T, T> chgReproduce;

    //是否已经全量加载过
    private volatile boolean fullloaded;

    //Entity信息
    final EntityInfo<T> info;

    //&#064;Cacheable的定时更新秒数，为0表示不定时更新
    final int interval;

    //&#064;Cacheable的定时器
    private ScheduledThreadPoolExecutor scheduler;

    public EntityCache(final EntityInfo<T> info, final Cacheable c) {
        this.info = info;
        this.interval = c == null ? 0 : c.interval();
        this.type = info.getType();
        this.creator = info.getCreator();
        this.primary = info.primary;
        VirtualEntity ve = info.getType().getAnnotation(VirtualEntity.class);
        this.needcopy = ve == null || !ve.direct();
        this.newReproduce = Reproduce.create(type, type, (m) -> {
            try {
                return type.getDeclaredField(m).getAnnotation(Transient.class) == null;
            } catch (Exception e) {
                return true;
            }
        });
        this.chgReproduce = Reproduce.create(type, type, (m) -> {
            try {
                java.lang.reflect.Field field = type.getDeclaredField(m);
                if (field.getAnnotation(Transient.class) != null) return false;
                Column column = field.getAnnotation(Column.class);
                return (column == null || column.updatable());
            } catch (Exception e) {
                return true;
            }
        });
    }

    public void fullLoad() {
        if (info.fullloader == null) return;
        this.fullloaded = false;
        ConcurrentHashMap newmap = new ConcurrentHashMap();
        List<T> all = info.fullloader.apply(info.source, type);
        if (all != null) {
            all.stream().filter(x -> x != null).forEach(x -> {
                newmap.put(this.primary.get(x), x);
            });
        }
        this.list = all == null ? new ConcurrentLinkedQueue() : new ConcurrentLinkedQueue(all);
        this.map = newmap;
        this.fullloaded = true;
        if (this.interval > 0) {
            this.scheduler = new ScheduledThreadPoolExecutor(1, (Runnable r) -> {
                final Thread t = new Thread(r, "EntityCache-" + type + "-Thread");
                t.setDaemon(true);
                return t;
            });
            this.scheduler.scheduleAtFixedRate(() -> {
                ConcurrentHashMap newmap2 = new ConcurrentHashMap();
                List<T> all2 = info.fullloader.apply(info.source, type);
                if (all2 != null) {
                    all2.stream().filter(x -> x != null).forEach(x -> {
                        newmap2.put(this.primary.get(x), x);
                    });
                }
                this.list = all2 == null ? new ConcurrentLinkedQueue() : new ConcurrentLinkedQueue(all2);
                this.map = newmap2;
            }, interval - System.currentTimeMillis() / 1000 % interval, interval, TimeUnit.SECONDS);
        }
    }

    public Class<T> getType() {
        return type;
    }

    public void clear() {
        this.fullloaded = false;
        this.list = new ConcurrentLinkedQueue();
        this.map = new ConcurrentHashMap();
        if (this.scheduler != null) {
            this.scheduler.shutdownNow();
            this.scheduler = null;
        }
    }

    public boolean isFullLoaded() {
        return fullloaded;
    }

    public T find(Serializable id) {
        if (id == null) return null;
        T rs = map.get(id);
        return rs == null ? null : (needcopy ? newReproduce.apply(this.creator.create(), rs) : rs);
    }

    public T find(final SelectColumn selects, final Serializable id) {
        if (id == null) return null;
        T rs = map.get(id);
        if (rs == null) return null;
        if (selects == null) return (needcopy ? newReproduce.apply(this.creator.create(), rs) : rs);
        T t = this.creator.create();
        for (Attribute attr : this.info.attributes) {
            if (selects.test(attr.field())) attr.set(t, attr.get(rs));
        }
        return t;
    }

    public T find(final SelectColumn selects, FilterNode node) {
        final Predicate<T> filter = node == null ? null : node.createPredicate(this);
        Stream<T> stream = this.list.stream();
        if (filter != null) stream = stream.filter(filter);
        Optional<T> opt = stream.findFirst();
        if (!opt.isPresent()) return null;
        if (selects == null) return (needcopy ? newReproduce.apply(this.creator.create(), opt.get()) : opt.get());
        T rs = opt.get();
        T t = this.creator.create();
        for (Attribute attr : this.info.attributes) {
            if (selects.test(attr.field())) attr.set(t, attr.get(rs));
        }
        return t;
    }

    public Serializable findColumn(final String column, final Serializable defValue, final Serializable id) {
        if (id == null) return defValue;
        T rs = map.get(id);
        if (rs == null) return defValue;
        for (Attribute attr : this.info.attributes) {
            if (column.equals(attr.field())) {
                Serializable val = (Serializable) attr.get(rs);
                return val == null ? defValue : val;
            }
        }
        return defValue;
    }

    public Serializable findColumn(final String column, final Serializable defValue, FilterNode node) {
        final Predicate<T> filter = node == null ? null : node.createPredicate(this);
        Stream<T> stream = this.list.stream();
        if (filter != null) stream = stream.filter(filter);
        Optional<T> opt = stream.findFirst();
        if (!opt.isPresent()) return defValue;
        T rs = opt.get();
        for (Attribute attr : this.info.attributes) {
            if (column.equals(attr.field())) {
                Serializable val = (Serializable) attr.get(rs);
                return val == null ? defValue : val;
            }
        }
        return defValue;
    }

    public boolean exists(Serializable id) {
        if (id == null) return false;
        final Class atype = this.primary.type();
        if (id.getClass() != atype && id instanceof Number) {
            if (atype == int.class || atype == Integer.class) {
                id = ((Number) id).intValue();
            } else if (atype == long.class || atype == Long.class) {
                id = ((Number) id).longValue();
            } else if (atype == short.class || atype == Short.class) {
                id = ((Number) id).shortValue();
            } else if (atype == float.class || atype == Float.class) {
                id = ((Number) id).floatValue();
            } else if (atype == byte.class || atype == Byte.class) {
                id = ((Number) id).byteValue();
            } else if (atype == double.class || atype == Double.class) {
                id = ((Number) id).doubleValue();
            } else if (atype == AtomicInteger.class) {
                id = new AtomicInteger(((Number) id).intValue());
            } else if (atype == AtomicLong.class) {
                id = new AtomicLong(((Number) id).longValue());
            }
        }
        return this.map.containsKey(id);
    }

    public boolean exists(FilterNode node) {
        final Predicate<T> filter = node == null ? null : node.createPredicate(this);
        Stream<T> stream = this.list.stream();
        if (filter != null) stream = stream.filter(filter);
        return stream.findFirst().isPresent();
    }

    public boolean exists(final Predicate<T> filter) {
        return (filter != null) && this.list.stream().filter(filter).findFirst().isPresent();
    }

    public <K, V> Map<Serializable, Number> queryColumnMap(final String keyColumn, final FilterFunc func, final String funcColumn, FilterNode node) {
        final Attribute<T, Serializable> keyAttr = info.getAttribute(keyColumn);
        final Predicate filter = node == null ? null : node.createPredicate(this);
        final Attribute funcAttr = funcColumn == null ? null : info.getAttribute(funcColumn);
        Stream<T> stream = this.list.stream();
        if (filter != null) stream = stream.filter(filter);
        Collector<T, Map, ?> collector = null;
        final Class valtype = funcAttr == null ? null : funcAttr.type();
        switch (func) {
            case AVG:
                if (valtype == float.class || valtype == Float.class || valtype == double.class || valtype == Double.class) {
                    collector = (Collector<T, Map, ?>) Collectors.averagingDouble((T t) -> ((Number) funcAttr.get(t)).doubleValue());
                } else {
                    collector = (Collector<T, Map, ?>) Collectors.averagingLong((T t) -> ((Number) funcAttr.get(t)).longValue());
                }
                break;
            case COUNT:
                collector = (Collector<T, Map, ?>) Collectors.counting();
                break;
            case DISTINCTCOUNT:
                collector = (Collector<T, Map, ?>) Collectors.mapping((t) -> funcAttr.get(t), Collectors.toSet());
                break;
            case MAX:
            case MIN:
                Comparator<T> comp = (o1, o2) -> o1 == null ? (o2 == null ? 0 : -1) : ((Comparable) funcAttr.get(o1)).compareTo(funcAttr.get(o2));
                collector = (Collector<T, Map, ?>) ((func == MAX) ? Collectors.maxBy(comp) : Collectors.minBy(comp));
                break;
            case SUM:
                if (valtype == float.class || valtype == Float.class || valtype == double.class || valtype == Double.class) {
                    collector = (Collector<T, Map, ?>) Collectors.summingDouble((T t) -> ((Number) funcAttr.get(t)).doubleValue());
                } else {
                    collector = (Collector<T, Map, ?>) Collectors.summingLong((T t) -> ((Number) funcAttr.get(t)).longValue());
                }
                break;
        }
        Map rs = stream.collect(Collectors.groupingBy(t -> keyAttr.get(t), LinkedHashMap::new, collector));
        if (func == MAX || func == MIN) {
            Map rs2 = new LinkedHashMap();
            rs.forEach((x, y) -> {
                if (((Optional) y).isPresent()) rs2.put(x, funcAttr.get((T) ((Optional) y).get()));
            });
            rs = rs2;
        } else if (func == DISTINCTCOUNT) {
            Map rs2 = new LinkedHashMap();
            rs.forEach((x, y) -> rs2.put(x, ((Set) y).size() + 0L));
            rs = rs2;
        }
        return rs;
    }

    public <V> Number getNumberResult(final FilterFunc func, final Number defResult, final String column, final FilterNode node) {
        final Attribute<T, Serializable> attr = column == null ? null : info.getAttribute(column);
        final Predicate<T> filter = node == null ? null : node.createPredicate(this);
        Stream<T> stream = this.list.stream();
        if (filter != null) stream = stream.filter(filter);
        switch (func) {
            case AVG:
                if (attr.type() == int.class || attr.type() == Integer.class || attr.type() == AtomicInteger.class) {
                    OptionalDouble rs = stream.mapToInt(x -> ((Number) attr.get(x)).intValue()).average();
                    return rs.isPresent() ? (int) rs.getAsDouble() : defResult;
                } else if (attr.type() == long.class || attr.type() == Long.class || attr.type() == AtomicLong.class) {
                    OptionalDouble rs = stream.mapToLong(x -> ((Number) attr.get(x)).longValue()).average();
                    return rs.isPresent() ? (long) rs.getAsDouble() : defResult;
                } else if (attr.type() == short.class || attr.type() == Short.class) {
                    OptionalDouble rs = stream.mapToInt(x -> ((Short) attr.get(x)).intValue()).average();
                    return rs.isPresent() ? (short) rs.getAsDouble() : defResult;
                } else if (attr.type() == float.class || attr.type() == Float.class) {
                    OptionalDouble rs = stream.mapToDouble(x -> ((Float) attr.get(x)).doubleValue()).average();
                    return rs.isPresent() ? (float) rs.getAsDouble() : defResult;
                } else if (attr.type() == double.class || attr.type() == Double.class) {
                    OptionalDouble rs = stream.mapToDouble(x -> (Double) attr.get(x)).average();
                    return rs.isPresent() ? rs.getAsDouble() : defResult;
                }
                throw new RuntimeException("getNumberResult error(type:" + type + ", attr.declaringClass: " + attr.declaringClass() + ", attr.field: " + attr.field() + ", attr.type: " + attr.type());
            case COUNT:
                return stream.count();
            case DISTINCTCOUNT:
                return stream.map(x -> attr.get(x)).distinct().count();

            case MAX:
                if (attr.type() == int.class || attr.type() == Integer.class || attr.type() == AtomicInteger.class) {
                    OptionalInt rs = stream.mapToInt(x -> ((Number) attr.get(x)).intValue()).max();
                    return rs.isPresent() ? rs.getAsInt() : defResult;
                } else if (attr.type() == long.class || attr.type() == Long.class || attr.type() == AtomicLong.class) {
                    OptionalLong rs = stream.mapToLong(x -> ((Number) attr.get(x)).longValue()).max();
                    return rs.isPresent() ? rs.getAsLong() : defResult;
                } else if (attr.type() == short.class || attr.type() == Short.class) {
                    OptionalInt rs = stream.mapToInt(x -> ((Short) attr.get(x)).intValue()).max();
                    return rs.isPresent() ? (short) rs.getAsInt() : defResult;
                } else if (attr.type() == float.class || attr.type() == Float.class) {
                    OptionalDouble rs = stream.mapToDouble(x -> ((Float) attr.get(x)).doubleValue()).max();
                    return rs.isPresent() ? (float) rs.getAsDouble() : defResult;
                } else if (attr.type() == double.class || attr.type() == Double.class) {
                    OptionalDouble rs = stream.mapToDouble(x -> (Double) attr.get(x)).max();
                    return rs.isPresent() ? rs.getAsDouble() : defResult;
                }
                throw new RuntimeException("getNumberResult error(type:" + type + ", attr.declaringClass: " + attr.declaringClass() + ", attr.field: " + attr.field() + ", attr.type: " + attr.type());

            case MIN:
                if (attr.type() == int.class || attr.type() == Integer.class || attr.type() == AtomicInteger.class) {
                    OptionalInt rs = stream.mapToInt(x -> ((Number) attr.get(x)).intValue()).min();
                    return rs.isPresent() ? rs.getAsInt() : defResult;
                } else if (attr.type() == long.class || attr.type() == Long.class || attr.type() == AtomicLong.class) {
                    OptionalLong rs = stream.mapToLong(x -> ((Number) attr.get(x)).longValue()).min();
                    return rs.isPresent() ? rs.getAsLong() : defResult;
                } else if (attr.type() == short.class || attr.type() == Short.class) {
                    OptionalInt rs = stream.mapToInt(x -> ((Short) attr.get(x)).intValue()).min();
                    return rs.isPresent() ? (short) rs.getAsInt() : defResult;
                } else if (attr.type() == float.class || attr.type() == Float.class) {
                    OptionalDouble rs = stream.mapToDouble(x -> ((Float) attr.get(x)).doubleValue()).min();
                    return rs.isPresent() ? (float) rs.getAsDouble() : defResult;
                } else if (attr.type() == double.class || attr.type() == Double.class) {
                    OptionalDouble rs = stream.mapToDouble(x -> (Double) attr.get(x)).min();
                    return rs.isPresent() ? rs.getAsDouble() : defResult;
                }
                throw new RuntimeException("getNumberResult error(type:" + type + ", attr.declaringClass: " + attr.declaringClass() + ", attr.field: " + attr.field() + ", attr.type: " + attr.type());

            case SUM:
                if (attr.type() == int.class || attr.type() == Integer.class || attr.type() == AtomicInteger.class) {
                    return stream.mapToInt(x -> ((Number) attr.get(x)).intValue()).sum();
                } else if (attr.type() == long.class || attr.type() == Long.class || attr.type() == AtomicLong.class) {
                    return stream.mapToLong(x -> ((Number) attr.get(x)).longValue()).sum();
                } else if (attr.type() == short.class || attr.type() == Short.class) {
                    return (short) stream.mapToInt(x -> ((Short) attr.get(x)).intValue()).sum();
                } else if (attr.type() == float.class || attr.type() == Float.class) {
                    return (float) stream.mapToDouble(x -> ((Float) attr.get(x)).doubleValue()).sum();
                } else if (attr.type() == double.class || attr.type() == Double.class) {
                    return stream.mapToDouble(x -> (Double) attr.get(x)).sum();
                }
                throw new RuntimeException("getNumberResult error(type:" + type + ", attr.declaringClass: " + attr.declaringClass() + ", attr.field: " + attr.field() + ", attr.type: " + attr.type());
        }
        return defResult;
    }

    public Sheet<T> querySheet(final SelectColumn selects, final Flipper flipper, final FilterNode node) {
        return querySheet(true, selects, flipper, node);
    }

    public Sheet<T> querySheet(final boolean needtotal, final SelectColumn selects, final Flipper flipper, FilterNode node) {
        final Predicate<T> filter = node == null ? null : node.createPredicate(this);
        final Comparator<T> comparator = createComparator(flipper);
        long total = 0;
        if (needtotal) {
            Stream<T> stream = this.list.stream();
            if (filter != null) stream = stream.filter(filter);
            total = stream.count();
        }
        if (needtotal && total == 0) return new Sheet<>();
        Stream<T> stream = this.list.stream();
        if (filter != null) stream = stream.filter(filter);
        if (comparator != null) stream = stream.sorted(comparator);
        if (flipper != null && flipper.getOffset() > 0) stream = stream.skip(flipper.getOffset());
        if (flipper != null && flipper.getLimit() > 0) stream = stream.limit(flipper.getLimit());
        final List<T> rs = new ArrayList<>();
        if (selects == null) {
            Consumer<? super T> action = x -> rs.add(needcopy ? newReproduce.apply(creator.create(), x) : x);
            if (comparator != null) {
                stream.forEachOrdered(action);
            } else {
                stream.forEach(action);
            }
        } else {
            final List<Attribute<T, Serializable>> attrs = new ArrayList<>();
            info.forEachAttribute((k, v) -> {
                if (selects.test(k)) attrs.add(v);
            });
            Consumer<? super T> action = x -> {
                final T item = creator.create();
                for (Attribute attr : attrs) {
                    attr.set(item, attr.get(x));
                }
                rs.add(item);
            };
            if (comparator != null) {
                stream.forEachOrdered(action);
            } else {
                stream.forEach(action);
            }
        }
        if (!needtotal) total = rs.size();
        return new Sheet<>(total, rs);
    }

    public int insert(T value) {
        if (value == null) return 0;
        final T rs = newReproduce.apply(this.creator.create(), value);  //确保同一主键值的map与list中的对象必须共用。
        T old = this.map.put(this.primary.get(rs), rs);
        if (old == null) {
            this.list.add(rs);
            return 1;
        } else {
            logger.log(Level.WARNING, this.type + " cache repeat insert data: " + value);
            return 0;
        }
    }

    public int delete(final Serializable id) {
        if (id == null) return 0;
        final T rs = this.map.remove(id);
        if (rs == null) return 0;
        this.list.remove(rs);
        return 1;
    }

    public Serializable[] delete(final Flipper flipper, final FilterNode node) {
        if (node == null || this.list.isEmpty()) return new Serializable[0];
        final Comparator<T> comparator = createComparator(flipper);
        Stream<T> stream = this.list.stream().filter(node.createPredicate(this));
        if (comparator != null) stream = stream.sorted(comparator);
        if (flipper != null && flipper.getOffset() > 0) stream = stream.skip(flipper.getOffset());
        if (flipper != null && flipper.getLimit() > 0) stream = stream.limit(flipper.getLimit());
        Object[] rms = stream.toArray();
        Serializable[] ids = new Serializable[rms.length];
        int i = -1;
        for (Object o : rms) {
            final T t = (T) o;
            ids[++i] = this.primary.get(t);
            this.map.remove(ids[i]);
            this.list.remove(t);
        }
        return ids;
    }

    public int update(final T value) {
        if (value == null) return 0;
        T rs = this.map.get(this.primary.get(value));
        if (rs == null) return 0;
        synchronized (rs) {
            this.chgReproduce.apply(rs, value);
        }
        return 1;
    }

    public T update(final T value, Collection<Attribute<T, Serializable>> attrs) {
        if (value == null) return value;
        T rs = this.map.get(this.primary.get(value));
        if (rs == null) return rs;
        synchronized (rs) {
            for (Attribute attr : attrs) {
                attr.set(rs, attr.get(value));
            }
        }
        return rs;
    }

    public T[] update(final T value, final Collection<Attribute<T, Serializable>> attrs, final FilterNode node) {
        if (value == null || node == null) return (T[]) Array.newInstance(type, 0);
        T[] rms = this.list.stream().filter(node.createPredicate(this)).toArray(len -> (T[]) Array.newInstance(type, len));
        for (T rs : rms) {
            synchronized (rs) {
                for (Attribute attr : attrs) {
                    attr.set(rs, attr.get(value));
                }
            }
        }
        return rms;
    }

    public <V> T update(final Serializable id, Attribute<T, V> attr, final V fieldValue) {
        if (id == null) return null;
        T rs = this.map.get(id);
        if (rs != null) attr.set(rs, fieldValue);
        return rs;
    }

    public <V> T[] update(Attribute<T, V> attr, final V fieldValue, final FilterNode node) {
        if (attr == null || node == null) return (T[]) Array.newInstance(type, 0);
        T[] rms = this.list.stream().filter(node.createPredicate(this)).toArray(len -> (T[]) Array.newInstance(type, len));
        for (T rs : rms) {
            attr.set(rs, fieldValue);
        }
        return rms;
    }

    public <V> T updateColumn(final Serializable id, List<Attribute<T, Serializable>> attrs, final List<ColumnValue> values) {
        if (id == null || attrs == null || attrs.isEmpty()) return null;
        T rs = this.map.get(id);
        if (rs == null) return rs;
        synchronized (rs) {
            for (int i = 0; i < attrs.size(); i++) {
                ColumnValue cv = values.get(i);
                updateColumn(attrs.get(i), rs, cv.getExpress(), cv.getValue());
            }
        }
        return rs;
    }

    public <V> T[] updateColumn(final FilterNode node, final Flipper flipper, List<Attribute<T, Serializable>> attrs, final List<ColumnValue> values) {
        if (attrs == null || attrs.isEmpty() || node == null) return (T[]) Array.newInstance(type, 0);
        Stream<T> stream = this.list.stream();
        final Comparator<T> comparator = createComparator(flipper);
        if (comparator != null) stream = stream.sorted(comparator);
        if (flipper != null && flipper.getLimit() > 0) stream = stream.limit(flipper.getLimit());
        T[] rms = stream.filter(node.createPredicate(this)).toArray(len -> (T[]) Array.newInstance(type, len));
        for (T rs : rms) {
            synchronized (rs) {
                for (int i = 0; i < attrs.size(); i++) {
                    ColumnValue cv = values.get(i);
                    updateColumn(attrs.get(i), rs, cv.getExpress(), cv.getValue());
                }
            }
        }
        return rms;
    }

    public <V> T updateColumnOr(final Serializable id, Attribute<T, V> attr, final long orvalue) {
        if (id == null) return null;
        T rs = this.map.get(id);
        if (rs == null) return rs;
        synchronized (rs) {
            return updateColumn(attr, rs, ColumnExpress.ORR, orvalue);
        }
    }

    public <V> T updateColumnAnd(final Serializable id, Attribute<T, V> attr, final long andvalue) {
        if (id == null) return null;
        T rs = this.map.get(id);
        if (rs == null) return rs;
        synchronized (rs) {
            return updateColumn(attr, rs, ColumnExpress.AND, andvalue);
        }
    }

    public <V> T updateColumnIncrement(final Serializable id, Attribute<T, V> attr, final long incvalue) {
        if (id == null) return null;
        T rs = this.map.get(id);
        if (rs == null) return rs;
        synchronized (rs) {
            return updateColumn(attr, rs, ColumnExpress.INC, incvalue);
        }
    }

    private <V> T updateColumn(Attribute<T, V> attr, final T rs, final ColumnExpress express, Serializable val) {
        final Class ft = attr.type();
        Number numb = null;
        Serializable newval = null;
        switch (express) {
            case INC:
                numb = (Number) attr.get(rs);
                if (numb == null) {
                    numb = (Number) val;
                } else {
                    numb = numb.longValue() + ((Number) val).longValue();
                }
                break;
            case MUL:
                numb = (Number) attr.get(rs);
                if (numb == null) {
                    numb = 0;
                } else {
                    numb = numb.longValue() * ((Number) val).floatValue();
                }
                break;
            case AND:
                numb = (Number) attr.get(rs);
                if (numb == null) {
                    numb = 0;
                } else {
                    numb = numb.longValue() & ((Number) val).longValue();
                }
                break;
            case ORR:
                numb = (Number) attr.get(rs);
                if (numb == null) {
                    numb = 0;
                } else {
                    numb = numb.longValue() | ((Number) val).longValue();
                }
                break;
            case MOV:
                newval = val;
                break;
        }
        if (numb != null) {
            if (ft == int.class || ft == Integer.class) {
                newval = numb.intValue();
            } else if (ft == long.class || ft == Long.class) {
                newval = numb.longValue();
            } else if (ft == short.class || ft == Short.class) {
                newval = numb.shortValue();
            } else if (ft == float.class || ft == Float.class) {
                newval = numb.floatValue();
            } else if (ft == double.class || ft == Double.class) {
                newval = numb.doubleValue();
            } else if (ft == byte.class || ft == Byte.class) {
                newval = numb.byteValue();
            } else if (ft == AtomicInteger.class) {
                newval = new AtomicInteger(numb.intValue());
            } else if (ft == AtomicLong.class) {
                newval = new AtomicLong(numb.longValue());
            }
        } else {
            if (ft == AtomicInteger.class && newval != null && newval.getClass() != AtomicInteger.class) {
                newval = new AtomicInteger(((Number) newval).intValue());
            } else if (ft == AtomicLong.class && newval != null && newval.getClass() != AtomicLong.class) {
                newval = new AtomicLong(((Number) newval).longValue());
            }
        }
        attr.set(rs, (V) newval);
        return rs;
    }

    public Attribute<T, Serializable> getAttribute(String fieldname) {
        return info.getAttribute(fieldname);
    }

    //-------------------------------------------------------------------------------------------------------------------------------
    protected Comparator<T> createComparator(Flipper flipper) {
        if (flipper == null || flipper.getSort() == null || flipper.getSort().isEmpty() || flipper.getSort().indexOf(';') >= 0 || flipper.getSort().indexOf('\n') >= 0) return null;
        final String sort = flipper.getSort();
        Comparator<T> comparator = this.sortComparators.get(sort);
        if (comparator != null) return comparator;
        for (String item : sort.split(",")) {
            if (item.trim().isEmpty()) continue;
            String[] sub = item.trim().split("\\s+");
            int pos = sub[0].indexOf('(');
            Attribute<T, Serializable> attr;
            if (pos <= 0) {
                attr = getAttribute(sub[0]);
            } else {  //含SQL函数
                int pos2 = sub[0].lastIndexOf(')');
                final Attribute<T, Serializable> pattr = getAttribute(sub[0].substring(pos + 1, pos2));
                final String func = sub[0].substring(0, pos);
                if ("ABS".equalsIgnoreCase(func)) {
                    Function getter = null;
                    if (pattr.type() == int.class || pattr.type() == Integer.class || pattr.type() == AtomicInteger.class) {
                        getter = x -> Math.abs(((Number) pattr.get((T) x)).intValue());
                    } else if (pattr.type() == long.class || pattr.type() == Long.class || pattr.type() == AtomicLong.class) {
                        getter = x -> Math.abs(((Number) pattr.get((T) x)).longValue());
                    } else if (pattr.type() == float.class || pattr.type() == Float.class) {
                        getter = x -> Math.abs(((Number) pattr.get((T) x)).floatValue());
                    } else if (pattr.type() == double.class || pattr.type() == Double.class) {
                        getter = x -> Math.abs(((Number) pattr.get((T) x)).doubleValue());
                    } else {
                        throw new RuntimeException("Flipper not supported sort illegal type by ABS (" + flipper.getSort() + ")");
                    }
                    attr = (Attribute<T, Serializable>) Attribute.create(pattr.declaringClass(), pattr.field(), pattr.type(), getter, (o, v) -> pattr.set(o, v));
                } else if (func.isEmpty()) {
                    attr = pattr;
                } else {
                    throw new RuntimeException("Flipper not supported sort illegal function (" + flipper.getSort() + ")");
                }
            }
            Comparator<T> c = (sub.length > 1 && sub[1].equalsIgnoreCase("DESC")) ? (T o1, T o2) -> {
                Comparable c1 = (Comparable) attr.get(o1);
                Comparable c2 = (Comparable) attr.get(o2);
                return c2 == null ? -1 : c2.compareTo(c1);
            } : (T o1, T o2) -> {
                Comparable c1 = (Comparable) attr.get(o1);
                Comparable c2 = (Comparable) attr.get(o2);
                return c1 == null ? -1 : c1.compareTo(c2);
            };

            if (comparator == null) {
                comparator = c;
            } else {
                comparator = comparator.thenComparing(c);
            }
        }
        this.sortComparators.put(sort, comparator);
        return comparator;
    }

    private static class UniqueSequence implements Serializable {

        private final Serializable[] value;

        public UniqueSequence(Serializable[] val) {
            this.value = val;
        }

        @Override
        public int hashCode() {
            return Arrays.deepHashCode(this.value);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) return false;
            if (getClass() != obj.getClass()) return false;
            final UniqueSequence other = (UniqueSequence) obj;
            if (value.length != other.value.length) return false;
            for (int i = 0; i < value.length; i++) {
                if (!value[i].equals(other.value[i])) return false;
            }
            return true;
        }

    }

    private static interface UniqueAttribute<T> extends Predicate<FilterNode> {

        public Serializable getValue(T bean);

        @Override
        public boolean test(FilterNode node);

        public static <T> UniqueAttribute<T> create(final Attribute<T, Serializable>[] attributes) {
            if (attributes.length == 1) {
                final Attribute<T, Serializable> attribute = attributes[0];
                return new UniqueAttribute<T>() {

                    @Override
                    public Serializable getValue(T bean) {
                        return attribute.get(bean);
                    }

                    @Override
                    public boolean test(FilterNode node) {
                        if (node == null || node.isOr()) return false;
                        if (!attribute.field().equals(node.column)) return false;
                        if (node.nodes == null) return true;
                        for (FilterNode n : node.nodes) {
                            if (!test(n)) return false;
                        }
                        return true;
                    }
                };
            } else {
                return new UniqueAttribute<T>() {

                    @Override
                    public Serializable getValue(T bean) {
                        final Serializable[] rs = new Serializable[attributes.length];
                        for (int i = 0; i < rs.length; i++) {
                            rs[i] = attributes[i].get(bean);
                        }
                        return new UniqueSequence(rs);
                    }

                    @Override
                    public boolean test(FilterNode node) {
                        return true;
                    }
                };
            }
        }
    }

}
