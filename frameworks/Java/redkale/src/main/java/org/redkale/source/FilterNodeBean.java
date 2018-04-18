/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.io.Serializable;
import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import javax.persistence.Transient;
import static org.redkale.source.FilterExpress.*;
import org.redkale.util.*;

/**
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <T> FilterBean泛型
 */
public final class FilterNodeBean<T extends FilterBean> implements Comparable<FilterNodeBean<T>> {

    private static final ConcurrentHashMap<Class, FilterNodeBean> beanodes = new ConcurrentHashMap<>();

    private Attribute<T, Serializable> beanAttr;

    private String column;

    private FilterExpress express;

    private boolean itemand;

    private boolean or;

    private FilterNodeBean[] nodeBeans;

    //-----------------join table--------------------------
    private Class joinClass;

    private String[] joinColumns;

    //----------------------------------------------------
    private long least;

    private boolean string;

    private boolean number;

    public FilterNodeBean(FilterNodeBean bean) {
        this.beanAttr = bean == null ? null : bean.beanAttr;
        this.column = bean == null ? null : bean.column;
        this.express = bean == null ? null : bean.express;
        this.itemand = bean == null ? true : bean.itemand;
        this.joinClass = bean == null ? null : bean.joinClass;
        this.joinColumns = bean == null ? null : bean.joinColumns;
        this.least = bean == null ? 1 : bean.least;
        this.string = bean == null ? false : bean.string;
        this.number = bean == null ? false : bean.number;
        this.or = bean == null ? false : bean.or;
        this.nodeBeans = bean == null ? null : bean.nodeBeans;
    }

    private FilterNodeBean(final FilterJoinColumn joinCol, final FilterColumn filterCol, final Attribute<T, Serializable> attr, final Type genericType) {
        this.beanAttr = attr;
        this.joinClass = joinCol == null ? null : joinCol.table();
        this.joinColumns = joinCol == null ? null : joinCol.columns();

        final Class type = attr.type();
        this.column = (filterCol != null && !filterCol.name().isEmpty()) ? filterCol.name() : attr.field();

        FilterExpress exp = filterCol == null ? null : filterCol.express();
        Class compType = type.getComponentType();
        if (Collection.class.isAssignableFrom(type) && genericType instanceof ParameterizedType) {
            Type pt = ((ParameterizedType) genericType).getActualTypeArguments()[0];
            if (pt instanceof Class) compType = (Class) pt;
        }
        if ((exp == null || exp == EQUAL) && (type.isArray() || Collection.class.isAssignableFrom(type))) {
            if (compType != null && Range.class.isAssignableFrom(compType)) {
                if (AND != exp) exp = OR;
            } else if (NOTIN != exp) {
                exp = IN;
            }
        } else if (Range.class.isAssignableFrom(type)) {
            if (NOTBETWEEN != exp) exp = BETWEEN;
        }
        if (exp == null) exp = EQUAL;
        this.express = exp;

        this.least = filterCol == null ? 1 : filterCol.least();
        this.number = (type.isPrimitive() && type != boolean.class) || Number.class.isAssignableFrom(type);
        this.string = CharSequence.class.isAssignableFrom(type);
    }

    private FilterNodeBean or(FilterNodeBean node) {
        return any(node, true);
    }

    private FilterNodeBean and(FilterNodeBean node) {
        return any(node, false);
    }

    private FilterNodeBean any(FilterNodeBean node, boolean signor) {
        Objects.requireNonNull(node);
        if (this.column == null) {
            this.beanAttr = node.beanAttr;
            this.column = node.column;
            this.express = node.express;
            this.itemand = node.itemand;
            this.joinClass = node.joinClass;
            this.joinColumns = node.joinColumns;
            this.least = node.least;
            this.string = node.string;
            this.number = node.number;
            return this;
        }
        if (this.nodeBeans == null) {
            this.nodeBeans = new FilterNodeBean[]{node};
            this.or = signor;
            return this;
        }
        if (or == signor) {
            this.nodeBeans = Utility.append(this.nodeBeans, node);
            return this;
        }
        this.nodeBeans = new FilterNodeBean[]{new FilterNodeBean(this), node};
        this.column = null;
        this.or = signor;
        return this;
    }

    public static FilterNode createFilterNode(final FilterBean bean) {
        if (bean == null) return null;
        return load(bean.getClass()).create(bean);
    }

    private FilterNode create(final T bean) {
        if (bean == null || beanAttr == null) return null;
        FilterNode node = null;
        final Serializable val = beanAttr.get(bean);
        if (column != null && val != null) {
            boolean skip = false;
            if (string && ((CharSequence) val).length() == 0) { //空字符串不需要进行过滤
                skip = true;
            } else if (number && ((Number) val).longValue() < least) { //数值小于过滤下值限则不需要过滤
                skip = true;
            }
            if (!skip) {
                if (this.joinClass == null) {
                    node = FilterNode.create(column, express, itemand, val);
                } else {
                    node = FilterJoinNode.create(joinClass, joinColumns, column, express, itemand, val);
                }
            }
        }
        if (this.nodeBeans == null) return node;
        for (final FilterNodeBean fnb : this.nodeBeans) {
            FilterNode n = fnb.create(bean);
            if (n == null) continue;
            node = node == null ? n : ((!(n instanceof FilterJoinNode)) ? n.any(node, or) : node.any(n, or));
        }
        return node;
    }

    private static <T extends FilterBean> FilterNodeBean createFilterNodeBean(final Class<T> clazz) {
        final Set<String> fields = new HashSet<>();
        final Map<String, FilterNodeBean> nodemap = new LinkedHashMap();
        Class cltmp = clazz;
        do {
            for (final Field field : cltmp.getDeclaredFields()) {
                if (Modifier.isStatic(field.getModifiers())) continue;
                if (fields.contains(field.getName())) continue;
                if (field.getAnnotation(Transient.class) != null) continue;

                final boolean pubmod = Modifier.isPublic(field.getModifiers());

                char[] chars = field.getName().toCharArray();
                chars[0] = Character.toUpperCase(chars[0]);
                final Class t = field.getType();
                Method getter = null;
                try {
                    getter = cltmp.getMethod(((t == boolean.class || t == Boolean.class) ? "is" : "get") + new String(chars));
                } catch (Exception ex) {
                    if (t == Boolean.class) {
                        try {
                            getter = cltmp.getMethod("get" + new String(chars));
                        } catch (Exception ex2) {
                            if (!pubmod) continue;
                        }
                    } else if (!pubmod) continue;
                }
                fields.add(field.getName());

                final Attribute<T, Serializable> beanAttr = pubmod ? Attribute.create(field) : Attribute.create(getter, null);
                FilterNodeBean<T> nodeBean = new FilterNodeBean(field.getAnnotation(FilterJoinColumn.class), field.getAnnotation(FilterColumn.class), beanAttr, field.getGenericType());

                //------------------------------------
                {
                    FilterGroup[] refs = field.getAnnotationsByType(FilterGroup.class);
                    String[] groups = new String[refs.length];
                    for (int i = 0; i < refs.length; i++) {
                        groups[i] = refs[i].value();
                    }
                    if (groups.length == 0) groups = new String[]{"[AND]"};
                    for (String key : groups) {
                        if (!key.startsWith("[AND]") && !key.startsWith("[OR]")) {
                            throw new RuntimeException(field + "'s FilterGroup.value(" + key + ") illegal, must be [AND] or [OR] startsWith");
                        }
                        FilterNodeBean node = nodemap.get(key);
                        if (node == null) {
                            nodemap.put(key, nodeBean);
                        } else if (nodeBean.joinClass == null && node.joinClass != null) { //非joinNode 关联 joinNode
                            nodemap.put(key, nodeBean.any(node, key.substring(key.lastIndexOf('.') + 1).contains("[OR]")));
                        } else {
                            node.any(nodeBean, key.substring(key.lastIndexOf('.') + 1).contains("[OR]"));
                        }
                    }
                }
            }
        } while ((cltmp = cltmp.getSuperclass()) != Object.class);
        final Map<String, LinkNode> linkes = new LinkedHashMap<>();
        nodemap.forEach((k, v) -> {
            String[] keys = k.split("\\.");
            LinkNode link = linkes.get(keys[0]);
            if (link == null) {
                linkes.put(keys[0], new LinkNode(k, v));
            } else {
                link.put(keys, 0, v);
            }
        });
        FilterNodeBean rs = null;
        for (LinkNode link : linkes.values()) {
            FilterNodeBean f = link.createFilterNodeBean();
            if (f == null) continue;
            rs = rs == null ? f : rs.and(f);
        }
        if (rs != null && rs.nodeBeans != null) Arrays.sort(rs.nodeBeans);
        return rs == null ? new FilterNodeBean(null) : rs;
    }

    @Override
    public int compareTo(FilterNodeBean<T> o) {
        if (this.joinClass == null && o.joinClass == null) return 0;
        if (this.joinClass != null && o.joinClass != null) return 0;
        return this.joinClass == null ? -1 : 1;
    }

    private static class LinkNode {

        public final boolean or;

        public final String key;

        public final List<FilterNodeBean> beans = new ArrayList<>();

        public final Map<String, LinkNode> nexts = new LinkedHashMap<>();

        public LinkNode(String keyString, FilterNodeBean node) {
            String[] keys = keyString.split("\\.");
            this.key = keys[0];
            this.or = this.key.contains("[OR]");
            put(keys, 0, node);
        }

        public LinkNode(String[] keyStrings, int pos, FilterNodeBean node) {
            this.key = keyStrings[pos];
            this.or = this.key.contains("[OR]");
            put(keyStrings, pos, node);
        }

        public FilterNodeBean createFilterNodeBean() {
            FilterNodeBean node = null;
            for (FilterNodeBean bean : beans) {
                node = node == null ? bean : node.any(bean, or);
            }
            for (LinkNode link : nexts.values()) {
                FilterNodeBean f = link.createFilterNodeBean();
                if (f == null) continue;
                node = node == null ? f : node.any(f, or);
            }
            return node;
        }

        public final void put(final String[] keys, int pos, final FilterNodeBean node) {
            if (keys.length == pos + 1 && this.key.equals(keys[pos])) {
                this.beans.add(node);
                return;
            }
            LinkNode link = nexts.get(keys[pos + 1]);
            if (link == null) {
                nexts.put(keys[pos + 1], new LinkNode(keys, pos + 1, node));
            } else {
                link.put(keys, pos + 1, node);
            }
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("{key = '").append(key).append("', or = ").append(or);
            if (!beans.isEmpty()) {
                sb.append(", beans = [\r\n");
                for (FilterNodeBean bean : this.beans) {
                    sb.append("    ").append(bean).append("\r\n");
                }
                sb.append("]");
            }
            if (!nexts.isEmpty()) {
                sb.append(", nexts = [\r\n");
                for (LinkNode link : this.nexts.values()) {
                    sb.append("    ").append(link).append("\r\n");
                }
                sb.append("]");
            }
            sb.append("}");
            return sb.toString();
        }
    }

    private static FilterNodeBean load(Class<? extends FilterBean> clazz) {
        FilterNodeBean rs = beanodes.get(clazz);
        if (rs != null) return rs;
        synchronized (beanodes) {
            rs = beanodes.get(clazz);
            if (rs == null) {
                rs = createFilterNodeBean(clazz);
                beanodes.put(clazz, rs);
            }
            return rs;
        }
    }

    @Override
    public String toString() {
        return toString(joinClass == null ? null : joinClass.getSimpleName()).toString();
    }

    protected StringBuilder toString(final String prefix) {
        StringBuilder sb = new StringBuilder();
        StringBuilder element = toElementString(prefix);
        boolean more = element.length() > 0 && this.nodeBeans != null;
        if (more) sb.append('(');
        sb.append(element);
        if (this.nodeBeans != null) {
            for (FilterNodeBean node : this.nodeBeans) {
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
        StringBuilder sb = new StringBuilder();
        if (column != null) {
            String col = prefix == null ? column : (prefix + "." + column);
            if (express == ISNULL || express == ISNOTNULL) {
                sb.append(col).append(' ').append(express.value());
            } else if (express == ISEMPTY || express == ISNOTEMPTY) {
                sb.append(col).append(' ').append(express.value()).append(" ''");
            } else {
                boolean lower = (express == IGNORECASEEQUAL || express == IGNORECASENOTEQUAL || express == IGNORECASELIKE
                    || express == IGNORECASENOTLIKE || express == IGNORECASECONTAIN || express == IGNORECASENOTCONTAIN);
                sb.append(lower ? ("LOWER(" + col + ')') : col).append(' ').append(express.value()).append(" ?");
            }
        }
        return sb;
    }
}
