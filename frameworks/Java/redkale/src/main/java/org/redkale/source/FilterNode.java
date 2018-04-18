/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.*;
import java.util.function.*;
import static org.redkale.source.FilterExpress.*;
import org.redkale.util.*;

/**
 * 注意： <br>
 * column的值以#开头的视为虚拟字段，不在过滤范围内 <br>
 * 在调用 createSQLExpress 之前必须先调用 createSQLJoin <br>
 * 在调用 createPredicate 之前必须先调用 isCacheUseable  <br>
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public class FilterNode {  //FilterNode 不能实现Serializable接口， 否则DataSource很多重载接口会出现冲突

    protected String column;

    protected FilterExpress express;

    protected Serializable value;

    protected boolean itemand;

    //----------------------------------------------
    protected boolean or;

    protected FilterNode[] nodes;

    public FilterNode() {
    }

    protected FilterNode(String col, FilterExpress exp, boolean itemand, Serializable val) {
        Objects.requireNonNull(col);
        if (exp == null) {
            if (val instanceof Range) {
                exp = FilterExpress.BETWEEN;
            } else if (val instanceof Collection) {
                if (!((Collection) val).isEmpty()) {
                    Object subval = null;
                    for (Object obj : (Collection) val) {  //取第一个值
                        subval = obj;
                        break;
                    }
                    if (subval instanceof Range) {
                        exp = FilterExpress.BETWEEN;
//                    } else if (subval instanceof Collection) {
//                        exp = FilterExpress.IN;
//                    } else if (subval != null && val.getClass().isArray()) {
//                        exp = FilterExpress.IN;
                    } else {
                        exp = FilterExpress.IN;
                    }
                } else { //空集合
                    exp = FilterExpress.IN;
                }
            } else if (val != null && val.getClass().isArray()) {
                Class comp = val.getClass().getComponentType();
                if (Range.class.isAssignableFrom(comp)) {
                    exp = FilterExpress.BETWEEN;
                } else {
                    exp = FilterExpress.IN;
                }
            }
        }
        this.column = col;
        this.express = exp == null ? EQUAL : exp;
        this.itemand = itemand;
        this.value = val;
    }

    public long findLongValue(final String col, long defValue) {
        Serializable val = findValue(col);
        return val == null ? defValue : ((Number) val).longValue();
    }

    public int findIntValue(final String col, int defValue) {
        Serializable val = findValue(col);
        return val == null ? defValue : ((Number) val).intValue();
    }

    public String findStringValue(final String col) {
        return (String) findValue(col);
    }

    public Serializable findValue(final String col) {
        if (this.column.equals(col)) return this.value;
        if (this.nodes == null) return null;
        for (FilterNode n : this.nodes) {
            if (n == null) continue;
            Serializable val = n.findValue(col);
            if (val != null) return val;
        }
        return null;
    }

    public final FilterNode and(FilterNode node) {
        return any(node, false);
    }

    public final FilterNode and(String column, Serializable value) {
        return and(column, null, value);
    }

    public final FilterNode and(String column, FilterExpress express, Serializable value) {
        return and(column, express, true, value);
    }

    public final FilterNode and(String column, FilterExpress express, boolean itemand, Serializable value) {
        return and(new FilterNode(column, express, itemand, value));
    }

    public final FilterNode or(FilterNode node) {
        return any(node, true);
    }

    public final FilterNode or(String column, Serializable value) {
        return or(column, null, value);
    }

    public final FilterNode or(String column, FilterExpress express, Serializable value) {
        return or(column, express, true, value);
    }

    public final FilterNode or(String column, FilterExpress express, boolean itemand, Serializable value) {
        return or(new FilterNode(column, express, itemand, value));
    }

    protected FilterNode any(FilterNode node, boolean signor) {
        Objects.requireNonNull(node);
        if (this.column == null) {
            this.column = node.column;
            this.express = node.express;
            this.itemand = node.itemand;
            this.value = node.value;
            return this;
        }
        if (this.nodes == null) {
            this.nodes = new FilterNode[]{node};
            this.or = signor;
            return this;
        }
        if (or == signor) {
            this.nodes = Utility.append(this.nodes, node);
            return this;
        }
        FilterNode newnode = new FilterNode(this.column, this.express, this.itemand, this.value);
        newnode.or = this.or;
        newnode.nodes = this.nodes;
        this.nodes = new FilterNode[]{newnode, node};
        this.column = null;
        this.express = null;
        this.itemand = true;
        this.or = signor;
        this.value = null;
        return this;
    }

    /**
     * 该方法需要重载
     *
     * @param <T>         Entity类的泛型
     * @param func        EntityInfo的加载器
     * @param update      是否用于更新的JOIN
     * @param joinTabalis 关联表集合
     * @param haset       已拼接过的字段名
     * @param info        Entity类的EntityInfo
     *
     * @return SQL的join语句 不存在返回null
     */
    protected <T> CharSequence createSQLJoin(final Function<Class, EntityInfo> func, final boolean update, final Map<Class, String> joinTabalis, final Set<String> haset, final EntityInfo<T> info) {
        if (joinTabalis == null || this.nodes == null) return null;
        StringBuilder sb = null;
        for (FilterNode node : this.nodes) {
            CharSequence cs = node.createSQLJoin(func, update, joinTabalis, haset, info);
            if (cs == null) continue;
            if (sb == null) sb = new StringBuilder();
            sb.append(cs);
        }
        return sb;
    }

    /**
     * 该方法需要重载
     *
     * @return 是否存在关联表
     */
    protected boolean isjoin() {
        if (this.nodes == null) return false;
        for (FilterNode node : this.nodes) {
            if (node.isjoin()) return true;
        }
        return false;
    }

    protected final Map<Class, String> getJoinTabalis() {
        if (!isjoin()) return null;
        Map<Class, String> map = new HashMap<>();
        putJoinTabalis(map);
        return map;
    }

    protected void putJoinTabalis(Map<Class, String> map) {
        if (this.nodes == null) return;
        for (FilterNode node : this.nodes) {
            node.putJoinTabalis(map);
        }
    }

    /**
     * 该方法需要重载
     *
     * @param entityApplyer EntityInfo的加载器
     *
     * @return 是否可以使用缓存
     */
    protected boolean isCacheUseable(final Function<Class, EntityInfo> entityApplyer) {
        if (this.nodes == null) return true;
        for (FilterNode node : this.nodes) {
            if (!node.isCacheUseable(entityApplyer)) return false;
        }
        return true;
    }

    /**
     * 该方法需要重载
     *
     * @param <T>         Entity类的泛型
     * @param joinTabalis 关联表的集合
     * @param info        EntityInfo
     *
     * @return JOIN的SQL语句
     */
    protected <T> CharSequence createSQLExpress(final EntityInfo<T> info, final Map<Class, String> joinTabalis) {
        CharSequence sb0 = this.column == null || this.column.isEmpty() || this.column.charAt(0) == '#' || info == null
            ? null : createElementSQLExpress(info, joinTabalis == null ? null : joinTabalis.get(info.getType()));
        if (this.nodes == null) return sb0;
        final StringBuilder rs = new StringBuilder();
        rs.append('(');
        boolean more = false;
        if (sb0 != null && sb0.length() > 2) {
            more = true;
            rs.append(sb0);
        }
        for (FilterNode node : this.nodes) {
            CharSequence f = node.createSQLExpress(info, joinTabalis);
            if (f == null || f.length() < 3) continue;
            if (more) rs.append(or ? " OR " : " AND ");
            rs.append(f);
            more = true;
        }
        rs.append(')');
        if (rs.length() < 5) return null;
        return rs;
    }

    public static FilterNode create(String column, Serializable value) {
        return create(column, null, value);
    }

    public static FilterNode create(String column, FilterExpress express, Serializable value) {
        return create(column, express, true, value);
    }

    public static FilterNode create(String column, FilterExpress express, boolean itemand, Serializable value) {
        return new FilterNode(column, express, itemand, value);
    }

    private boolean needSplit(final Object val0) {
        return needSplit(express, val0);
    }

    private static boolean needSplit(final FilterExpress express, final Object val0) {
        if (val0 == null) return false;
        boolean items = express != IN && express != NOTIN;  //是否数组集合的表达式
        if (!items) {
            if (val0.getClass().isArray()) {
                Class comp = val0.getClass().getComponentType();
                if (!(comp.isPrimitive() || CharSequence.class.isAssignableFrom(comp) || Number.class.isAssignableFrom(comp))) {
                    items = true;
                }
            } else if (val0 instanceof Collection) {
                for (Object fv : (Collection) val0) {
                    if (fv == null) continue;
                    Class comp = fv.getClass();
                    if (!(comp.isPrimitive() || CharSequence.class.isAssignableFrom(comp) || Number.class.isAssignableFrom(comp))) {
                        items = true;
                    }
                    break;  //只需检测第一个值
                }
            }
        }
        return items;
    }

    protected final <T> CharSequence createElementSQLExpress(final EntityInfo<T> info, String talis) {
        final Object val0 = getValue();
        if (needSplit(val0)) {
            if (val0 instanceof Collection) {
                StringBuilder sb = new StringBuilder();
                boolean more = ((Collection) val0).size() > 1;
                if (more) sb.append('(');
                for (Object fv : (Collection) val0) {
                    if (fv == null) continue;
                    CharSequence cs = createElementSQLExpress(info, talis, fv);
                    if (cs == null) continue;
                    if (sb.length() > 2) sb.append(itemand ? " AND " : " OR ");
                    sb.append(cs);
                }
                if (more) sb.append(')');
                return sb.length() > 3 ? sb : null;  //若sb的值只是()，则不过滤
            } else if (val0.getClass().isArray()) {
                StringBuilder sb = new StringBuilder();
                Object[] fvs = (Object[]) val0;
                boolean more = fvs.length > 1;
                if (more) sb.append('(');
                for (Object fv : fvs) {
                    if (fv == null) continue;
                    CharSequence cs = createElementSQLExpress(info, talis, fv);
                    if (cs == null) continue;
                    if (sb.length() > 2) sb.append(itemand ? " AND " : " OR ");
                    sb.append(cs);
                }
                if (more) sb.append(')');
                return sb.length() > 3 ? sb : null;  //若sb的值只是()，则不过滤
            }
        }
        return createElementSQLExpress(info, talis, val0);

    }

    private <T> CharSequence createElementSQLExpress(final EntityInfo<T> info, String talis, Object val0) {
        if (column == null || this.column.isEmpty() || this.column.charAt(0) == '#') return null;
        if (talis == null) talis = "a";
        if (express == ISNULL || express == ISNOTNULL) {
            return new StringBuilder().append(info.getSQLColumn(talis, column)).append(' ').append(express.value());
        }
        if (express == ISEMPTY || express == ISNOTEMPTY) {
            return new StringBuilder().append(info.getSQLColumn(talis, column)).append(' ').append(express.value()).append(" ''");
        }
        if (val0 == null) return null;
        if (express == FV_MOD || express == FV_DIV) {
            FilterValue fv = (FilterValue) val0;
            return new StringBuilder().append(info.getSQLColumn(talis, column)).append(' ').append(express.value()).append(' ').append(fv.getOptvalue())
                .append(' ').append(fv.getExpress().value()).append(' ').append(fv.getDestvalue());
        }
        final boolean fk = (val0 instanceof FilterKey);
        CharSequence val = fk ? info.getSQLColumn(talis, ((FilterKey) val0).getColumn()) : formatToString(express, val0);
        if (val == null) return null;
        StringBuilder sb = new StringBuilder(32);
        if (express == CONTAIN) return info.containSQL.replace("${column}", info.getSQLColumn(talis, column)).replace("${keystr}", val);
        if (express == IGNORECASECONTAIN) return info.containSQL.replace("${column}", "LOWER(" + info.getSQLColumn(talis, column) + ")").replace("${keystr}", val);
        if (express == NOTCONTAIN) return info.notcontainSQL.replace("${column}", info.getSQLColumn(talis, column)).replace("${keystr}", val);
        if (express == IGNORECASENOTCONTAIN) return info.notcontainSQL.replace("${column}", "LOWER(" + info.getSQLColumn(talis, column) + ")").replace("${keystr}", val);

        if (express == IGNORECASEEQUAL || express == IGNORECASENOTEQUAL || express == IGNORECASELIKE || express == IGNORECASENOTLIKE) {
            sb.append("LOWER(").append(info.getSQLColumn(talis, column)).append(')');
            if (fk) val = "LOWER(" + info.getSQLColumn(talis, ((FilterKey) val0).getColumn()) + ')';
        } else {
            sb.append(info.getSQLColumn(talis, column));
        }
        sb.append(' ');
        switch (express) {
            case OPAND:
            case OPOR:
                sb.append(express.value()).append(' ').append(val).append(" > 0");
                break;
            case OPANDNO:
                sb.append(express.value()).append(' ').append(val).append(" = 0");
                break;
            default:
                sb.append(express.value()).append(' ').append(val);
                break;
        }
        return sb;
    }

    protected <T, E> Predicate<T> createPredicate(final EntityCache<T> cache) {
        if (cache == null || (column == null && this.nodes == null)) return null;
        Predicate<T> filter = createElementPredicate(cache, false);
        if (this.nodes == null) return filter;
        for (FilterNode node : this.nodes) {
            Predicate<T> f = node.createPredicate(cache);
            if (f == null) continue;
            final Predicate<T> one = filter;
            final Predicate<T> two = f;
            filter = (filter == null) ? f : (or ? new Predicate<T>() {

                @Override
                public boolean test(T t) {
                    return one.test(t) || two.test(t);
                }

                @Override
                public String toString() {
                    return "(" + one + " OR " + two + ")";
                }
            } : new Predicate<T>() {

                @Override
                public boolean test(T t) {
                    return one.test(t) && two.test(t);
                }

                @Override
                public String toString() {
                    return "(" + one + " AND " + two + ")";
                }
            });
        }
        return filter;
    }

    protected final <T> Predicate<T> createElementPredicate(final EntityCache<T> cache, final boolean join) {
        if (this.column == null || this.column.isEmpty() || this.column.charAt(0) == '#') return null;
        return createElementPredicate(cache, join, cache.getAttribute(column));
    }

    @SuppressWarnings("unchecked")
    protected final <T> Predicate<T> createElementPredicate(final EntityCache<T> cache, final boolean join, final Attribute<T, Serializable> attr) {
        final Object val0 = getValue();
        if (needSplit(val0)) {
            if (val0 instanceof Collection) {
                Predicate<T> filter = null;
                for (Object fv : (Collection) val0) {
                    if (fv == null) continue;
                    Predicate<T> f = createElementPredicate(cache, join, attr, fv);
                    if (f == null) continue;
                    final Predicate<T> one = filter;
                    final Predicate<T> two = f;
                    filter = (filter == null) ? f : (!itemand ? new Predicate<T>() {

                        @Override
                        public boolean test(T t) {
                            return one.test(t) || two.test(t);
                        }

                        @Override
                        public String toString() {
                            return "(" + one + " OR " + two + ")";
                        }
                    } : new Predicate<T>() {

                        @Override
                        public boolean test(T t) {
                            return one.test(t) && two.test(t);
                        }

                        @Override
                        public String toString() {
                            return "(" + one + " AND " + two + ")";
                        }
                    });
                }
                return filter;
            } else if (val0.getClass().isArray()) {
                final Class primtype = val0.getClass();
                Object val2 = val0;
                int ix = -1;
                if (primtype == boolean[].class) {
                    boolean[] bs = (boolean[]) val0;
                    Boolean[] ns = new Boolean[bs.length];
                    for (boolean v : bs) {
                        ns[++ix] = v;
                    }
                    val2 = ns;
                } else if (primtype == byte[].class) {
                    byte[] bs = (byte[]) val0;
                    Byte[] ns = new Byte[bs.length];
                    for (byte v : bs) {
                        ns[++ix] = v;
                    }
                    val2 = ns;
                } else if (primtype == short[].class) {
                    short[] bs = (short[]) val0;
                    Short[] ns = new Short[bs.length];
                    for (short v : bs) {
                        ns[++ix] = v;
                    }
                    val2 = ns;
                } else if (primtype == char[].class) {
                    char[] bs = (char[]) val0;
                    Character[] ns = new Character[bs.length];
                    for (char v : bs) {
                        ns[++ix] = v;
                    }
                    val2 = ns;
                } else if (primtype == int[].class) {
                    int[] bs = (int[]) val0;
                    Integer[] ns = new Integer[bs.length];
                    for (int v : bs) {
                        ns[++ix] = v;
                    }
                    val2 = ns;
                } else if (primtype == float[].class) {
                    float[] bs = (float[]) val0;
                    Float[] ns = new Float[bs.length];
                    for (float v : bs) {
                        ns[++ix] = v;
                    }
                    val2 = ns;
                } else if (primtype == long[].class) {
                    long[] bs = (long[]) val0;
                    Long[] ns = new Long[bs.length];
                    for (long v : bs) {
                        ns[++ix] = v;
                    }
                    val2 = ns;
                } else if (primtype == double[].class) {
                    double[] bs = (double[]) val0;
                    Double[] ns = new Double[bs.length];
                    for (double v : bs) {
                        ns[++ix] = v;
                    }
                    val2 = ns;
                }
                Predicate<T> filter = null;
                for (Object fv : (Object[]) val2) {
                    if (fv == null) continue;
                    Predicate<T> f = createElementPredicate(cache, join, attr, fv);
                    if (f == null) continue;
                    final Predicate<T> one = filter;
                    final Predicate<T> two = f;
                    filter = (filter == null) ? f : (!itemand ? new Predicate<T>() {

                        @Override
                        public boolean test(T t) {
                            return one.test(t) || two.test(t);
                        }

                        @Override
                        public String toString() {
                            return "(" + one + " OR " + two + ")";
                        }
                    } : new Predicate<T>() {

                        @Override
                        public boolean test(T t) {
                            return one.test(t) && two.test(t);
                        }

                        @Override
                        public String toString() {
                            return "(" + one + " AND " + two + ")";
                        }
                    });
                }
                return filter;
            }
        }
        return createElementPredicate(cache, join, attr, val0);
    }

    @SuppressWarnings("unchecked")
    protected final <T> Predicate<T> createElementPredicate(final EntityCache<T> cache, final boolean join, final Attribute<T, Serializable> attr, Object val0) {
        if (attr == null) return null;
        final String field = join ? (cache.getType().getSimpleName() + "." + attr.field()) : attr.field();
        if (express == ISNULL) return new Predicate<T>() {

                @Override
                public boolean test(T t) {
                    return attr.get(t) == null;
                }

                @Override
                public String toString() {
                    return field + " = null";
                }
            };
        if (express == ISNOTNULL) return new Predicate<T>() {

                @Override
                public boolean test(T t) {
                    return attr.get(t) != null;
                }

                @Override
                public String toString() {
                    return field + " != null";
                }
            };
        if (express == ISEMPTY) return new Predicate<T>() {

                @Override
                public boolean test(T t) {
                    Object v = attr.get(t);
                    return v == null || v.toString().isEmpty();
                }

                @Override
                public String toString() {
                    return field + " = ''";
                }
            };
        if (express == ISNOTEMPTY) return new Predicate<T>() {

                @Override
                public boolean test(T t) {
                    Object v = attr.get(t);
                    return v != null && !v.toString().isEmpty();
                }

                @Override
                public String toString() {
                    return field + " != ''";
                }
            };
        if (val0 == null) return null;

        final Class atype = attr.type();
        final Class valtype = val0.getClass();
        if (atype != valtype && val0 instanceof Number) {
            if (atype == int.class || atype == Integer.class) {
                val0 = ((Number) val0).intValue();
            } else if (atype == long.class || atype == Long.class) {
                val0 = ((Number) val0).longValue();
            } else if (atype == short.class || atype == Short.class) {
                val0 = ((Number) val0).shortValue();
            } else if (atype == float.class || atype == Float.class) {
                val0 = ((Number) val0).floatValue();
            } else if (atype == byte.class || atype == Byte.class) {
                val0 = ((Number) val0).byteValue();
            } else if (atype == double.class || atype == Double.class) {
                val0 = ((Number) val0).doubleValue();
            }
        } else if (valtype.isArray()) {
            final int len = Array.getLength(val0);
            if (len == 0 && express == NOTIN) return null;
            final Class compType = valtype.getComponentType();
            if (atype != compType && len > 0) {
                if (!compType.isPrimitive() && Number.class.isAssignableFrom(compType)) throw new RuntimeException("param(" + val0 + ") type not match " + atype + " for column " + column);
                if (atype == int.class || atype == Integer.class) {
                    int[] vs = new int[len];
                    for (int i = 0; i < len; i++) {
                        vs[i] = ((Number) Array.get(val0, i)).intValue();
                    }
                    val0 = vs;
                } else if (atype == long.class || atype == Long.class) {
                    long[] vs = new long[len];
                    for (int i = 0; i < len; i++) {
                        vs[i] = ((Number) Array.get(val0, i)).longValue();
                    }
                    val0 = vs;
                } else if (atype == short.class || atype == Short.class) {
                    short[] vs = new short[len];
                    for (int i = 0; i < len; i++) {
                        vs[i] = ((Number) Array.get(val0, i)).shortValue();
                    }
                    val0 = vs;
                } else if (atype == float.class || atype == Float.class) {
                    float[] vs = new float[len];
                    for (int i = 0; i < len; i++) {
                        vs[i] = ((Number) Array.get(val0, i)).floatValue();
                    }
                    val0 = vs;
                } else if (atype == byte.class || atype == Byte.class) {
                    byte[] vs = new byte[len];
                    for (int i = 0; i < len; i++) {
                        vs[i] = ((Number) Array.get(val0, i)).byteValue();
                    }
                    val0 = vs;
                } else if (atype == double.class || atype == Double.class) {
                    double[] vs = new double[len];
                    for (int i = 0; i < len; i++) {
                        vs[i] = ((Number) Array.get(val0, i)).doubleValue();
                    }
                    val0 = vs;
                }
            }
        } else if (val0 instanceof Collection) {
            final Collection collection = (Collection) val0;
            if (collection.isEmpty() && express == NOTIN) return null;
            if (!collection.isEmpty()) {
                Iterator it = collection.iterator();
                it.hasNext();
                Class fs = it.next().getClass();
                Class pfs = fs;
                if (fs == Integer.class) {
                    pfs = int.class;
                } else if (fs == Long.class) {
                    pfs = long.class;
                } else if (fs == Short.class) {
                    pfs = short.class;
                } else if (fs == Float.class) {
                    pfs = float.class;
                } else if (fs == Byte.class) {
                    pfs = byte.class;
                } else if (fs == Double.class) {
                    pfs = double.class;
                }
                if (Number.class.isAssignableFrom(fs) && atype != fs && atype != pfs) { //需要转换
                    ArrayList list = new ArrayList(collection.size());
                    if (atype == int.class || atype == Integer.class) {
                        for (Number num : (Collection<Number>) collection) {
                            list.add(num.intValue());
                        }
                    } else if (atype == long.class || atype == Long.class) {
                        for (Number num : (Collection<Number>) collection) {
                            list.add(num.longValue());
                        }
                    } else if (atype == short.class || atype == Short.class) {
                        for (Number num : (Collection<Number>) collection) {
                            list.add(num.shortValue());
                        }
                    } else if (atype == float.class || atype == Float.class) {
                        for (Number num : (Collection<Number>) collection) {
                            list.add(num.floatValue());
                        }
                    } else if (atype == byte.class || atype == Byte.class) {
                        for (Number num : (Collection<Number>) collection) {
                            list.add(num.byteValue());
                        }
                    } else if (atype == double.class || atype == Double.class) {
                        for (Number num : (Collection<Number>) collection) {
                            list.add(num.doubleValue());
                        }
                    }
                    val0 = list;
                }
            }
        }
        final Serializable val = (Serializable) val0;
        final boolean fk = (val instanceof FilterKey);
        final Attribute<T, Serializable> fkattr = fk ? cache.getAttribute(((FilterKey) val).getColumn()) : null;
        if (fk && fkattr == null) throw new RuntimeException(cache.getType() + " not found column(" + ((FilterKey) val).getColumn() + ")");
        switch (express) {
            case EQUAL:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return Objects.equals(fkattr.get(t), attr.get(t));
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + fkattr.field();
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return val.equals(attr.get(t));
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + formatToString(val);
                    }
                };
            case IGNORECASEEQUAL:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        Object rs2 = fkattr.get(t);
                        if (rs == null && rs2 == null) return true;
                        if (rs == null || rs2 == null) return false;
                        return Objects.equals(rs.toString().toLowerCase(), rs2.toString().toLowerCase());
                    }

                    @Override
                    public String toString() {
                        return "LOWER(" + field + ") " + express.value() + " LOWER(" + fkattr.field() + ')';
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        if (rs == null) return false;
                        return val.toString().equalsIgnoreCase(rs.toString());
                    }

                    @Override
                    public String toString() {
                        return "LOWER(" + field + ") " + express.value() + ' ' + formatToString(val);
                    }
                };
            case NOTEQUAL:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return !Objects.equals(fkattr.get(t), attr.get(t));
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + fkattr.field();
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return !val.equals(attr.get(t));
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + formatToString(val);
                    }
                };
            case IGNORECASENOTEQUAL:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        Object rs2 = fkattr.get(t);
                        if (rs == null && rs2 == null) return false;
                        if (rs == null || rs2 == null) return true;
                        return !Objects.equals(rs.toString().toLowerCase(), rs2.toString().toLowerCase());
                    }

                    @Override
                    public String toString() {
                        return "LOWER(" + field + ") " + express.value() + " LOWER(" + fkattr.field() + ')';
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        if (rs == null) return true;
                        return !val.toString().equalsIgnoreCase(rs.toString());
                    }

                    @Override
                    public String toString() {
                        return "LOWER(" + field + ") " + express.value() + ' ' + formatToString(val);
                    }
                };
            case GREATERTHAN:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return ((Number) attr.get(t)).longValue() > ((Number) fkattr.get(t)).longValue();
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + fkattr.field();
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return ((Number) attr.get(t)).longValue() > ((Number) val).longValue();
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + val;
                    }
                };
            case LESSTHAN:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return ((Number) attr.get(t)).longValue() < ((Number) fkattr.get(t)).longValue();
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + fkattr.field();
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return ((Number) attr.get(t)).longValue() < ((Number) val).longValue();
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + val;
                    }
                };
            case GREATERTHANOREQUALTO:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return ((Number) attr.get(t)).longValue() >= ((Number) fkattr.get(t)).longValue();
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + fkattr.field();
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return ((Number) attr.get(t)).longValue() >= ((Number) val).longValue();
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + val;
                    }
                };
            case LESSTHANOREQUALTO:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return ((Number) attr.get(t)).longValue() <= ((Number) fkattr.get(t)).longValue();
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + fkattr.field();
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return ((Number) attr.get(t)).longValue() <= ((Number) val).longValue();
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + val;
                    }
                };

            case FV_MOD:
                FilterValue fv0 = (FilterValue) val;
                switch (fv0.getExpress()) {
                    case EQUAL:
                        return new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return (((Number) attr.get(t)).longValue() % fv0.getOptvalue().longValue()) == fv0.getDestvalue().longValue();
                            }

                            @Override
                            public String toString() {
                                return field + " " + express.value() + " " + fv0.getOptvalue() + " " + fv0.getExpress().value() + " " + fv0.getDestvalue();
                            }
                        };
                    case NOTEQUAL:
                        return new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return (((Number) attr.get(t)).longValue() % fv0.getOptvalue().longValue()) != fv0.getDestvalue().longValue();
                            }

                            @Override
                            public String toString() {
                                return field + " " + express.value() + " " + fv0.getOptvalue() + " " + fv0.getExpress().value() + " " + fv0.getDestvalue();
                            }
                        };
                    case GREATERTHAN:
                        return new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return (((Number) attr.get(t)).longValue() % fv0.getOptvalue().longValue()) > fv0.getDestvalue().longValue();
                            }

                            @Override
                            public String toString() {
                                return field + " " + express.value() + " " + fv0.getOptvalue() + " " + fv0.getExpress().value() + " " + fv0.getDestvalue();
                            }
                        };
                    case LESSTHAN:
                        return new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return (((Number) attr.get(t)).longValue() % fv0.getOptvalue().longValue()) < fv0.getDestvalue().longValue();
                            }

                            @Override
                            public String toString() {
                                return field + " " + express.value() + " " + fv0.getOptvalue() + " " + fv0.getExpress().value() + " " + fv0.getDestvalue();
                            }
                        };
                    case GREATERTHANOREQUALTO:
                        return new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return (((Number) attr.get(t)).longValue() % fv0.getOptvalue().longValue()) >= fv0.getDestvalue().longValue();
                            }

                            @Override
                            public String toString() {
                                return field + " " + express.value() + " " + fv0.getOptvalue() + " " + fv0.getExpress().value() + " " + fv0.getDestvalue();
                            }
                        };
                    case LESSTHANOREQUALTO:
                        return new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return (((Number) attr.get(t)).longValue() % fv0.getOptvalue().longValue()) <= fv0.getDestvalue().longValue();
                            }

                            @Override
                            public String toString() {
                                return field + " " + express.value() + " " + fv0.getOptvalue() + " " + fv0.getExpress().value() + " " + fv0.getDestvalue();
                            }
                        };
                    default:
                        throw new RuntimeException("(" + fv0 + ")'s express illegal, must be =, !=, <, >, <=, >=");
                }
            case FV_DIV:
                FilterValue fv1 = (FilterValue) val;
                switch (fv1.getExpress()) {
                    case EQUAL:
                        return new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return (((Number) attr.get(t)).longValue() / fv1.getOptvalue().longValue()) == fv1.getDestvalue().longValue();
                            }

                            @Override
                            public String toString() {
                                return field + " " + express.value() + " " + fv1.getOptvalue() + " " + fv1.getExpress().value() + " " + fv1.getDestvalue();
                            }
                        };
                    case NOTEQUAL:
                        return new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return (((Number) attr.get(t)).longValue() / fv1.getOptvalue().longValue()) != fv1.getDestvalue().longValue();
                            }

                            @Override
                            public String toString() {
                                return field + " " + express.value() + " " + fv1.getOptvalue() + " " + fv1.getExpress().value() + " " + fv1.getDestvalue();
                            }
                        };
                    case GREATERTHAN:
                        return new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return (((Number) attr.get(t)).longValue() / fv1.getOptvalue().longValue()) > fv1.getDestvalue().longValue();
                            }

                            @Override
                            public String toString() {
                                return field + " " + express.value() + " " + fv1.getOptvalue() + " " + fv1.getExpress().value() + " " + fv1.getDestvalue();
                            }
                        };
                    case LESSTHAN:
                        return new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return (((Number) attr.get(t)).longValue() / fv1.getOptvalue().longValue()) < fv1.getDestvalue().longValue();
                            }

                            @Override
                            public String toString() {
                                return field + " " + express.value() + " " + fv1.getOptvalue() + " " + fv1.getExpress().value() + " " + fv1.getDestvalue();
                            }
                        };
                    case GREATERTHANOREQUALTO:
                        return new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return (((Number) attr.get(t)).longValue() / fv1.getOptvalue().longValue()) >= fv1.getDestvalue().longValue();
                            }

                            @Override
                            public String toString() {
                                return field + " " + express.value() + " " + fv1.getOptvalue() + " " + fv1.getExpress().value() + " " + fv1.getDestvalue();
                            }
                        };
                    case LESSTHANOREQUALTO:
                        return new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return (((Number) attr.get(t)).longValue() / fv1.getOptvalue().longValue()) <= fv1.getDestvalue().longValue();
                            }

                            @Override
                            public String toString() {
                                return field + " " + express.value() + " " + fv1.getOptvalue() + " " + fv1.getExpress().value() + " " + fv1.getDestvalue();
                            }
                        };
                    default:
                        throw new RuntimeException("(" + fv1 + ")'s express illegal, must be =, !=, <, >, <=, >=");
                }
            case OPAND:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return (((Number) attr.get(t)).longValue() & ((Number) fkattr.get(t)).longValue()) > 0;
                    }

                    @Override
                    public String toString() {
                        return field + " & " + fkattr.field() + " > 0";
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return (((Number) attr.get(t)).longValue() & ((Number) val).longValue()) > 0;
                    }

                    @Override
                    public String toString() {
                        return field + " & " + val + " > 0";
                    }
                };
            case OPOR:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return (((Number) attr.get(t)).longValue() | ((Number) fkattr.get(t)).longValue()) > 0;
                    }

                    @Override
                    public String toString() {
                        return field + " | " + fkattr.field() + " > 0";
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return (((Number) attr.get(t)).longValue() | ((Number) val).longValue()) > 0;
                    }

                    @Override
                    public String toString() {
                        return field + " | " + val + " > 0";
                    }
                };
            case OPANDNO:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return (((Number) attr.get(t)).longValue() & ((Number) fkattr.get(t)).longValue()) == 0;
                    }

                    @Override
                    public String toString() {
                        return field + " & " + fkattr.field() + " = 0";
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        return (((Number) attr.get(t)).longValue() & ((Number) val).longValue()) == 0;
                    }

                    @Override
                    public String toString() {
                        return field + " & " + val + " = 0";
                    }
                };
            case LIKE:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        Object rs2 = fkattr.get(t);
                        return rs != null && rs2 != null && rs.toString().contains(rs2.toString());
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + fkattr.field();
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        return rs != null && rs.toString().contains(val.toString());
                    }

                    @Override
                    public String toString() {
                        return field + ' ' + express.value() + ' ' + formatToString(val);
                    }
                };
            case STARTSWITH:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        Object rs2 = fkattr.get(t);
                        return rs != null && rs2 != null && rs.toString().startsWith(rs2.toString());
                    }

                    @Override
                    public String toString() {
                        return field + " STARTSWITH " + fkattr.field();
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        return rs != null && rs.toString().startsWith(val.toString());
                    }

                    @Override
                    public String toString() {
                        return field + " STARTSWITH " + formatToString(val);
                    }
                };
            case ENDSWITH:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        Object rs2 = fkattr.get(t);
                        return rs != null && rs2 != null && rs.toString().endsWith(rs2.toString());
                    }

                    @Override
                    public String toString() {
                        return field + " ENDSWITH " + fkattr.field();
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        return rs != null && rs.toString().endsWith(val.toString());
                    }

                    @Override
                    public String toString() {
                        return field + " ENDSWITH " + formatToString(val);
                    }
                };
            case IGNORECASELIKE:
                if (fk) return new Predicate<T>() {

                        @Override
                        public boolean test(T t) {
                            Object rs = attr.get(t);
                            Object rs2 = fkattr.get(t);
                            return rs != null && rs2 != null && rs.toString().toLowerCase().contains(rs2.toString().toLowerCase());
                        }

                        @Override
                        public String toString() {
                            return "LOWER(" + field + ") " + express.value() + " LOWER(" + fkattr.field() + ')';
                        }
                    };
                final String valstr = val.toString().toLowerCase();
                return new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        return rs != null && rs.toString().toLowerCase().contains(valstr);
                    }

                    @Override
                    public String toString() {
                        return "LOWER(" + field + ") " + express.value() + ' ' + formatToString(valstr);
                    }
                };
            case NOTSTARTSWITH:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        Object rs2 = fkattr.get(t);
                        return rs == null || rs2 == null || !rs.toString().startsWith(rs2.toString());
                    }

                    @Override
                    public String toString() {
                        return field + " NOT STARTSWITH " + fkattr.field();
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        return rs == null || !rs.toString().startsWith(val.toString());
                    }

                    @Override
                    public String toString() {
                        return field + " NOT STARTSWITH " + formatToString(val);
                    }
                };
            case NOTENDSWITH:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        Object rs2 = fkattr.get(t);
                        return rs == null || rs2 == null || !rs.toString().endsWith(rs2.toString());
                    }

                    @Override
                    public String toString() {
                        return field + " NOT ENDSWITH " + fkattr.field();
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        return rs == null || !rs.toString().endsWith(val.toString());
                    }

                    @Override
                    public String toString() {
                        return field + " NOT ENDSWITH " + formatToString(val);
                    }
                };
            case IGNORECASENOTLIKE:
                if (fk) return new Predicate<T>() {

                        @Override
                        public boolean test(T t) {
                            Object rs = attr.get(t);
                            Object rs2 = fkattr.get(t);
                            return rs == null || rs2 == null || !rs.toString().toLowerCase().contains(rs2.toString().toLowerCase());
                        }

                        @Override
                        public String toString() {
                            return "LOWER(" + field + ") " + express.value() + " LOWER(" + fkattr.field() + ')';
                        }
                    };
                final String valstr2 = val.toString().toLowerCase();
                return new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        return rs == null || !rs.toString().toLowerCase().contains(valstr2);
                    }

                    @Override
                    public String toString() {
                        return "LOWER(" + field + ") " + express.value() + ' ' + formatToString(valstr2);
                    }
                };
            case CONTAIN:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        Object rs2 = fkattr.get(t);
                        return rs != null && rs2 != null && rs2.toString().contains(rs.toString());
                    }

                    @Override
                    public String toString() {
                        return fkattr.field() + ' ' + express.value() + ' ' + field;
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        return rs != null && val.toString().contains(rs.toString());
                    }

                    @Override
                    public String toString() {
                        return "" + formatToString(val) + ' ' + express.value() + ' ' + field;
                    }
                };
            case IGNORECASECONTAIN:
                if (fk) return new Predicate<T>() {

                        @Override
                        public boolean test(T t) {
                            Object rs = attr.get(t);
                            Object rs2 = fkattr.get(t);
                            return rs != null && rs2 != null && rs2.toString().toLowerCase().contains(rs.toString().toLowerCase());
                        }

                        @Override
                        public String toString() {
                            return " LOWER(" + fkattr.field() + ") " + express.value() + ' ' + "LOWER(" + field + ") ";
                        }
                    };
                final String valstr3 = val.toString().toLowerCase();
                return new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        return rs != null && valstr3.contains(rs.toString().toLowerCase());
                    }

                    @Override
                    public String toString() {
                        return "" + formatToString(valstr3) + express.value() + ' ' + "LOWER(" + field + ") ";
                    }
                };
            case NOTCONTAIN:
                return fk ? new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        Object rs2 = fkattr.get(t);
                        return rs == null || rs2 == null || !rs2.toString().contains(rs.toString());
                    }

                    @Override
                    public String toString() {
                        return fkattr.field() + ' ' + express.value() + ' ' + field;
                    }
                } : new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        return rs == null || !val.toString().contains(rs.toString());
                    }

                    @Override
                    public String toString() {
                        return "" + formatToString(val) + ' ' + express.value() + ' ' + field;
                    }
                };
            case IGNORECASENOTCONTAIN:
                if (fk) return new Predicate<T>() {

                        @Override
                        public boolean test(T t) {
                            Object rs = attr.get(t);
                            Object rs2 = fkattr.get(t);
                            return rs == null || rs2 == null || !rs2.toString().toLowerCase().contains(rs.toString().toLowerCase());
                        }

                        @Override
                        public String toString() {
                            return " LOWER(" + fkattr.field() + ") " + express.value() + ' ' + "LOWER(" + field + ") ";
                        }
                    };
                final String valstr4 = val.toString().toLowerCase();
                return new Predicate<T>() {

                    @Override
                    public boolean test(T t) {
                        Object rs = attr.get(t);
                        return rs == null || !valstr4.contains(rs.toString().toLowerCase());
                    }

                    @Override
                    public String toString() {
                        return "" + formatToString(valstr4) + express.value() + ' ' + "LOWER(" + field + ") ";
                    }
                };
            case BETWEEN:
            case NOTBETWEEN:
                Range range = (Range) val;
                final Comparable min = range.getMin();
                final Comparable max = range.getMax();
                if (express == BETWEEN) return new Predicate<T>() {

                        @Override
                        public boolean test(T t) {
                            Comparable rs = (Comparable) attr.get(t);
                            if (rs == null) return false;
                            if (min != null && min.compareTo(rs) >= 0) return false;
                            return !(max != null && max.compareTo(rs) <= 0);
                        }

                        @Override
                        public String toString() {
                            return field + " BETWEEN " + min + " AND " + max;
                        }
                    };
                if (express == NOTBETWEEN) return new Predicate<T>() {

                        @Override
                        public boolean test(T t) {
                            Comparable rs = (Comparable) attr.get(t);
                            if (rs == null) return true;
                            if (min != null && min.compareTo(rs) >= 0) return true;
                            return (max != null && max.compareTo(rs) <= 0);
                        }

                        @Override
                        public String toString() {
                            return field + " NOT BETWEEN " + min + " AND " + max;
                        }
                    };
                return null;
            case IN:
            case NOTIN:
                Predicate<T> filter;
                if (val instanceof Collection) {
                    Collection array = (Collection) val;
                    if (array.isEmpty()) { //express 只会是 IN
                        filter = new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return false;
                            }

                            @Override
                            public String toString() {
                                return field + ' ' + express.value() + " []";
                            }
                        };
                    } else {
                        filter = new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                Object rs = attr.get(t);
                                return rs != null && array.contains(rs);
                            }

                            @Override
                            public String toString() {
                                return field + ' ' + express.value() + ' ' + val;
                            }
                        };
                    }
                } else {
                    Class type = val.getClass();
                    if (Array.getLength(val) == 0) {//express 只会是 IN
                        filter = new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                return false;
                            }

                            @Override
                            public String toString() {
                                return field + ' ' + express.value() + " []";
                            }
                        };
                    } else if (type == int[].class) {
                        filter = new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                Object rs = attr.get(t);
                                if (rs == null) return false;
                                int k = (int) rs;
                                for (int v : (int[]) val) {
                                    if (v == k) return true;
                                }
                                return false;
                            }

                            @Override
                            public String toString() {
                                return field + ' ' + express.value() + ' ' + Arrays.toString((int[]) val);
                            }
                        };
                    } else if (type == short[].class) {
                        filter = new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                Object rs = attr.get(t);
                                if (rs == null) return false;
                                short k = (short) rs;
                                for (short v : (short[]) val) {
                                    if (v == k) return true;
                                }
                                return false;
                            }

                            @Override
                            public String toString() {
                                return field + ' ' + express.value() + ' ' + Arrays.toString((short[]) val);
                            }
                        };
                    } else if (type == long[].class) {
                        filter = new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                Object rs = attr.get(t);
                                if (rs == null) return false;
                                long k = (long) rs;
                                for (long v : (long[]) val) {
                                    if (v == k) return true;
                                }
                                return false;
                            }

                            @Override
                            public String toString() {
                                return field + ' ' + express.value() + ' ' + Arrays.toString((long[]) val);
                            }
                        };
                    } else if (type == float[].class) {
                        filter = new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                Object rs = attr.get(t);
                                if (rs == null) return false;
                                float k = (float) rs;
                                for (float v : (float[]) val) {
                                    if (v == k) return true;
                                }
                                return false;
                            }

                            @Override
                            public String toString() {
                                return field + ' ' + express.value() + ' ' + Arrays.toString((float[]) val);
                            }
                        };
                    } else if (type == double[].class) {
                        filter = new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                Object rs = attr.get(t);
                                if (rs == null) return false;
                                double k = (double) rs;
                                for (double v : (double[]) val) {
                                    if (v == k) return true;
                                }
                                return false;
                            }

                            @Override
                            public String toString() {
                                return field + ' ' + express.value() + ' ' + Arrays.toString((double[]) val);
                            }
                        };
                    } else {
                        filter = new Predicate<T>() {

                            @Override
                            public boolean test(T t) {
                                Object rs = attr.get(t);
                                if (rs == null) return false;
                                for (Object v : (Object[]) val) {
                                    if (rs.equals(v)) return true;
                                }
                                return false;
                            }

                            @Override
                            public String toString() {
                                return field + ' ' + express.value() + ' ' + Arrays.toString((Object[]) val);
                            }
                        };
                    }
                }
                if (express == NOTIN) {
                    final Predicate<T> filter2 = filter;
                    filter = new Predicate<T>() {

                        @Override
                        public boolean test(T t) {
                            return !filter2.test(t);
                        }

                        @Override
                        public String toString() {
                            return filter2.toString();
                        }
                    };
                }
                return filter;
        }
        return null;
    }

    @Override
    public String toString() {
        return toString(null).toString();
    }

    protected StringBuilder toString(final String prefix) {
        StringBuilder sb = new StringBuilder();
        StringBuilder element = toElementString(prefix);
        boolean more = element != null && element.length() > 0 && this.nodes != null;
        if (more) sb.append('(');
        sb.append(element);
        if (this.nodes != null) {
            for (FilterNode node : this.nodes) {
                String s = node.toString();
                if (s.length() < 1) continue;
                if (sb.length() > 1) sb.append(or ? " OR " : " AND ");
                sb.append(s);
            }
        }
        if (more) sb.append(')');
        return sb;
    }

    protected final StringBuilder toElementString(final String prefix) {
        Serializable val0 = getValue();
        if (needSplit(val0)) {
            if (val0 instanceof Collection) {
                StringBuilder sb = new StringBuilder();
                boolean more = ((Collection) val0).size() > 1;
                if (more) sb.append('(');
                for (Object fv : (Collection) val0) {
                    if (fv == null) continue;
                    CharSequence cs = toElementString(prefix, fv);
                    if (cs == null) continue;
                    if (sb.length() > 2) sb.append(itemand ? " AND " : " OR ");
                    sb.append(cs);
                }
                if (more) sb.append(')');
                return sb.length() > 3 ? sb : null;  //若sb的值只是()，则不过滤
            } else if (val0.getClass().isArray()) {
                StringBuilder sb = new StringBuilder();
                Object[] fvs = (Object[]) val0;
                boolean more = fvs.length > 1;
                if (more) sb.append('(');
                for (Object fv : fvs) {
                    if (fv == null) continue;
                    CharSequence cs = toElementString(prefix, fv);
                    if (cs == null) continue;
                    if (sb.length() > 2) sb.append(itemand ? " AND " : " OR ");
                    sb.append(cs);
                }
                if (more) sb.append(')');
                return sb.length() > 3 ? sb : null;  //若sb的值只是()，则不过滤
            }
        }
        return toElementString(prefix, val0);
    }

    protected final StringBuilder toElementString(final String prefix, Object ev) {
        StringBuilder sb = new StringBuilder();
        if (column != null) {
            String col = prefix == null ? column : (prefix + "." + column);
            if (express == ISNULL || express == ISNOTNULL) {
                sb.append(col).append(' ').append(express.value());
            } else if (express == ISEMPTY || express == ISNOTEMPTY) {
                sb.append(col).append(' ').append(express.value()).append(" ''");
            } else if (ev != null) {
                boolean lower = (express == IGNORECASELIKE || express == IGNORECASENOTLIKE || express == IGNORECASECONTAIN || express == IGNORECASENOTCONTAIN);
                sb.append(lower ? ("LOWER(" + col + ')') : col).append(' ').append(express.value()).append(' ').append(formatToString(express, ev));
            }
        }
        return sb;
    }

    protected static CharSequence formatToString(Object value) {
        CharSequence sb = formatToString(null, value);
        return sb == null ? null : sb.toString();
    }

    private static CharSequence formatToString(FilterExpress express, Object value) {
        if (value == null) return null;
        if (value instanceof Number) return String.valueOf(value);
        if (value instanceof CharSequence) {
            if (express == LIKE || express == NOTLIKE) {
                value = "%" + value + '%';
            } else if (express == STARTSWITH || express == NOTSTARTSWITH) {
                value = value + "%";
            } else if (express == ENDSWITH || express == NOTENDSWITH) {
                value = "%" + value;
            } else if (express == IGNORECASELIKE || express == IGNORECASENOTLIKE) {
                value = "%" + value.toString().toLowerCase() + '%';
            } else if (express == IGNORECASECONTAIN || express == IGNORECASENOTCONTAIN
                || express == IGNORECASEEQUAL || express == IGNORECASENOTEQUAL) {
                value = value.toString().toLowerCase();
            }
            return new StringBuilder().append('\'').append(value.toString().replace("'", "\\'")).append('\'');
        } else if (value instanceof Range) {
            Range range = (Range) value;
            boolean rangestring = range.getClass() == Range.StringRange.class;
            StringBuilder sb = new StringBuilder();
            if (rangestring) {
                sb.append('\'').append(range.getMin().toString().replace("'", "\\'")).append('\'');
            } else {
                sb.append(range.getMin());
            }
            sb.append(" AND ");
            if (rangestring) {
                sb.append('\'').append(range.getMax().toString().replace("'", "\\'")).append('\'');
            } else {
                sb.append(range.getMax());
            }
            return sb;
        } else if (value.getClass().isArray()) {
            int len = Array.getLength(value);
            if (len == 0) return express == NOTIN ? null : new StringBuilder("(NULL)");
            if (len == 1) {
                Object firstval = Array.get(value, 0);
                if (firstval != null && firstval.getClass().isArray()) return formatToString(express, firstval);
            }
            StringBuilder sb = new StringBuilder();
            sb.append('(');
            for (int i = 0; i < len; i++) {
                Object o = Array.get(value, i);
                if (sb.length() > 1) sb.append(',');
                if (o instanceof CharSequence) {
                    sb.append('\'').append(o.toString().replace("'", "\\'")).append('\'');
                } else {
                    sb.append(o);
                }
            }
            return sb.append(')');
        } else if (value instanceof Collection) {
            Collection c = (Collection) value;
            if (c.isEmpty()) return express == NOTIN ? null : new StringBuilder("(NULL)");
            StringBuilder sb = new StringBuilder();
            sb.append('(');
            for (Object o : c) {
                if (sb.length() > 1) sb.append(',');
                if (o instanceof CharSequence) {
                    sb.append('\'').append(o.toString().replace("'", "\\'")).append('\'');
                } else {
                    sb.append(o);
                }
            }
            return sb.append(')');
        }
        return String.valueOf(value);
    }

    public final Serializable getValue() {
        return value;
    }

    public final void setValue(Serializable value) {
        this.value = value;
    }

    public final boolean isOr() {
        return or;
    }

    public final void setOr(boolean or) {
        this.or = or;
    }

    public final String getColumn() {
        return column;
    }

    public final void setColumn(String column) {
        this.column = column;
    }

    public final FilterExpress getExpress() {
        return express;
    }

    public final void setExpress(FilterExpress express) {
        this.express = express;
    }

    public final boolean isItemand() {
        return itemand;
    }

    public final void setItemand(boolean itemand) {
        this.itemand = itemand;
    }

    public final FilterNode[] getNodes() {
        return nodes;
    }

    public final void setNodes(FilterNode[] nodes) {
        this.nodes = nodes;
    }

}
