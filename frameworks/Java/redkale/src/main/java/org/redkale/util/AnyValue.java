/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.util;

import java.lang.reflect.Array;
import java.util.*;
import java.util.function.*;
import org.redkale.convert.ConvertDisabled;

/**
 * 该类提供类似JSONObject的数据结构，主要用于读取xml配置文件和http-header存储
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@SuppressWarnings("unchecked")
public abstract class AnyValue {

    /**
     * 可读写的AnyValue默认实现类
     *
     * @author zhangjx
     */
    @SuppressWarnings("unchecked")
    public static final class DefaultAnyValue extends AnyValue {

        /**
         * 区分name大小写的比较策略
         *
         */
        public static final BiPredicate<String, String> EQUALS = (name1, name2) -> name1.equals(name2);

        /**
         * 不区分name大小写的比较策略
         */
        public static final BiPredicate<String, String> EQUALSIGNORE = (name1, name2) -> name1.equalsIgnoreCase(name2);

        private boolean ignoreCase;

        private BiPredicate<String, String> predicate;

        private Entry<String>[] stringEntrys = new Entry[0];

        private Entry<DefaultAnyValue>[] anyEntrys = new Entry[0];

        /**
         * 创建空的DefaultAnyValue对象
         *
         * @return DefaultAnyValue对象
         */
        public static final DefaultAnyValue create() {
            return new DefaultAnyValue();
        }

        /**
         * 创建含name-value值的DefaultAnyValue对象
         *
         * @param name  name
         * @param value value值
         *
         * @return DefaultAnyValue对象
         */
        public static final DefaultAnyValue create(String name, Number value) {
            DefaultAnyValue conf = new DefaultAnyValue();
            conf.addValue(name, value);
            return conf;
        }

        /**
         * 创建含name-value值的DefaultAnyValue对象
         *
         * @param name  name
         * @param value value值
         *
         * @return DefaultAnyValue对象
         */
        public static final DefaultAnyValue create(String name, String value) {
            DefaultAnyValue conf = new DefaultAnyValue();
            conf.addValue(name, value);
            return conf;
        }

        /**
         * 创建含name-value值的DefaultAnyValue对象
         *
         * @param name  name
         * @param value value值
         *
         * @return DefaultAnyValue对象
         */
        public static final DefaultAnyValue create(String name, AnyValue value) {
            DefaultAnyValue conf = new DefaultAnyValue();
            conf.addValue(name, value);
            return conf;
        }

        /**
         * 创建一个区分大小写比较策略的DefaultAnyValue对象
         *
         */
        public DefaultAnyValue() {
            this(false);
        }

        /**
         * 创建DefaultAnyValue对象
         *
         * @param ignoreCase name是否不区分大小写
         */
        public DefaultAnyValue(boolean ignoreCase) {
            this.ignoreCase = ignoreCase;
            this.predicate = ignoreCase ? EQUALSIGNORE : EQUALS;
        }

        /**
         * 创建共享此内容的DefaultAnyValue对象
         *
         * @return DefaultAnyValue对象
         */
        public DefaultAnyValue duplicate() {
            DefaultAnyValue rs = new DefaultAnyValue(this.ignoreCase);
            rs.stringEntrys = this.stringEntrys;
            rs.anyEntrys = this.anyEntrys;
            return rs;
        }

        public DefaultAnyValue addAll(final AnyValue av) {
            if (av == null) return this;
            if (av instanceof DefaultAnyValue) {
                final DefaultAnyValue adv = (DefaultAnyValue) av;
                if (adv.stringEntrys != null) {
                    for (Entry<String> en : adv.stringEntrys) {
                        this.addValue(en.name, en.value);
                    }
                }
                if (adv.anyEntrys != null) {
                    for (Entry<DefaultAnyValue> en : adv.anyEntrys) {
                        this.addValue(en.name, en.value);
                    }
                }
            } else {
                final Entry<String>[] strings = av.getStringEntrys();
                if (strings != null) {
                    for (Entry<String> en : strings) {
                        this.addValue(en.name, en.value);
                    }
                }
                final Entry<AnyValue>[] anys = av.getAnyEntrys();
                if (anys != null) {
                    for (Entry<AnyValue> en : anys) {
                        this.addValue(en.name, en.value);
                    }
                }
            }
            return this;
        }

        public DefaultAnyValue setAll(final AnyValue av) {
            if (av == null) return this;
            if (av instanceof DefaultAnyValue) {
                final DefaultAnyValue adv = (DefaultAnyValue) av;
                if (adv.stringEntrys != null) {
                    for (Entry<String> en : adv.stringEntrys) {
                        this.setValue(en.name, en.value);
                    }
                }
                if (adv.anyEntrys != null) {
                    for (Entry<DefaultAnyValue> en : adv.anyEntrys) {
                        this.setValue(en.name, en.value);
                    }
                }
            } else {
                final Entry<String>[] strings = av.getStringEntrys();
                if (strings != null) {
                    for (Entry<String> en : strings) {
                        this.setValue(en.name, en.value);
                    }
                }
                final Entry<AnyValue>[] anys = av.getAnyEntrys();
                if (anys != null) {
                    for (Entry<AnyValue> en : anys) {
                        this.setValue(en.name, en.value);
                    }
                }
            }
            return this;
        }

        @Override
        public void forEach(BiConsumer<String, String> stringConsumer) {
            forEach(stringConsumer, null);
        }

        @Override
        public void forEach(BiConsumer<String, String> stringConsumer, BiConsumer<String, AnyValue> anyConsumer) {
            if (stringConsumer != null) {
                for (Entry<String> en : stringEntrys) {
                    stringConsumer.accept(en.name, en.value);
                }
            }
            if (anyConsumer != null) {
                for (Entry<AnyValue> en : (Entry[]) anyEntrys) {
                    anyConsumer.accept(en.name, en.value);
                }
            }
        }

        @Override
        public Entry<String>[] getStringEntrys() {
            return stringEntrys;
        }

        public void setStringEntrys(Entry<String>[] stringEntrys) {
            this.stringEntrys = stringEntrys;
        }

        @Override
        public Entry<AnyValue>[] getAnyEntrys() {
            return (Entry<AnyValue>[]) (Entry[]) anyEntrys;
        }

        public void setAnyEntrys(Entry<DefaultAnyValue>[] anyEntrys) {
            this.anyEntrys = anyEntrys;
        }

        public boolean isIgnoreCase() {
            return ignoreCase;
        }

        public void setIgnoreCase(boolean ignoreCase) {
            this.ignoreCase = ignoreCase;
            if (this.predicate == null) {
                this.predicate = ignoreCase ? EQUALSIGNORE : EQUALS;
            }
        }

        @Override
        @ConvertDisabled
        public String[] getNames() {
            Set<String> set = new LinkedHashSet<>();
            for (Entry en : this.stringEntrys) {
                set.add(en.name);
            }
            for (Entry en : this.anyEntrys) {
                set.add(en.name);
            }
            return set.toArray(new String[set.size()]);
        }

        @Override
        public String[] getValues(String... names) {
            return Entry.getValues(this.predicate, String.class, this.stringEntrys, names);
        }

        @Override
        public AnyValue[] getAnyValues(String... names) {
            return Entry.getValues(this.predicate, DefaultAnyValue.class, this.anyEntrys, names);
        }

        @Override
        public String toString() {
            return toString(0);
        }

        public DefaultAnyValue clear() {
            this.stringEntrys = new Entry[0];
            this.anyEntrys = new Entry[0];
            return this;
        }

        public DefaultAnyValue setValue(String name, String value) {
            if (name == null) return this;
            if (getValue(name) == null) {
                this.addValue(name, value);
            } else {
                for (Entry<String> en : this.stringEntrys) {
                    if (predicate.test(en.name, name)) {
                        en.value = value;
                        return this;
                    }
                }
            }
            return this;
        }

        public DefaultAnyValue setValue(String name, AnyValue value) {
            if (name == null) return this;
            if (getValue(name) == null) {
                this.addValue(name, value);
            } else {
                for (Entry<DefaultAnyValue> en : this.anyEntrys) {
                    if (predicate.test(en.name, name)) {
                        en.value = (DefaultAnyValue) value;
                        return this;
                    }
                }
            }
            return this;
        }

        public DefaultAnyValue addValue(String name, boolean value) {
            return addValue(name, String.valueOf(value));
        }

        public DefaultAnyValue addValue(String name, Number value) {
            return addValue(name, String.valueOf(value));
        }

        public DefaultAnyValue addValue(String name, String value) {
            this.stringEntrys = Utility.append(this.stringEntrys, new Entry(name, value));
            return this;
        }

        public DefaultAnyValue addValue(String name, AnyValue value) {
            if (name == null || value == null) return this;
            this.anyEntrys = Utility.append(this.anyEntrys, new Entry(name, value));
            return this;
        }

        public DefaultAnyValue removeValue(String name, AnyValue value) {
            if (name == null || value == null || this.anyEntrys == null) return this;
            this.anyEntrys = Utility.remove(this.anyEntrys, (t) -> name.equals(((Entry) t).name) && ((Entry) t).getValue().equals(value));
            return this;
        }

        public DefaultAnyValue removeValue(String name, String value) {
            if (name == null || value == null || this.stringEntrys == null) return this;
            this.stringEntrys = Utility.remove(this.stringEntrys, (t) -> name.equals(((Entry) t).name) && ((Entry) t).getValue().equals(value));
            return this;
        }

        @Override
        public AnyValue getAnyValue(String name) {
            for (Entry<DefaultAnyValue> en : this.anyEntrys) {
                if (predicate.test(en.name, name)) {
                    return en.value;
                }
            }
            return null;
        }

        @Override
        public String getValue(String name) {
            for (Entry<String> en : this.stringEntrys) {
                if (predicate.test(en.name, name)) {
                    return en.value;
                }
            }
            return null;
        }

        @Override
        public String[] getValues(String name) {
            return Entry.getValues(this.predicate, String.class, this.stringEntrys, name);
        }

        @Override
        public AnyValue[] getAnyValues(String name) {
            return Entry.getValues(this.predicate, DefaultAnyValue.class, this.anyEntrys, name);
        }

    }

    public static final class Entry<T> {

        public final String name;

        T value;

        @ConstructorParameters({"name", "value"})
        public Entry(String name0, T value0) {
            this.name = name0;
            this.value = value0;
        }

        public String getName() {
            return name;
        }

        public T getValue() {
            return value;
        }

        static <T> T[] getValues(BiPredicate<String, String> comparison, Class<T> clazz, Entry<T>[] entitys, String name) {
            int len = 0;
            for (Entry en : entitys) {
                if (comparison.test(en.name, name)) {
                    ++len;
                }
            }
            if (len == 0) return (T[]) Array.newInstance(clazz, len);
            T[] rs = (T[]) Array.newInstance(clazz, len);
            int i = 0;
            for (Entry<T> en : entitys) {
                if (comparison.test(en.name, name)) {
                    rs[i++] = en.value;
                }
            }
            return rs;
        }

        static <T> T[] getValues(BiPredicate<String, String> comparison, Class<T> clazz, Entry<T>[] entitys, String... names) {
            int len = 0;
            for (Entry en : entitys) {
                for (String name : names) {
                    if (comparison.test(en.name, name)) {
                        ++len;
                        break;
                    }
                }
            }
            if (len == 0) return (T[]) Array.newInstance(clazz, len);
            T[] rs = (T[]) Array.newInstance(clazz, len);
            int i = 0;
            for (Entry<T> en : entitys) {
                for (String name : names) {
                    if (comparison.test(en.name, name)) {
                        rs[i++] = en.value;
                        break;
                    }
                }
            }
            return rs;
        }
    }

    public static DefaultAnyValue create() {
        return new DefaultAnyValue();
    }

    public String toString(int indent) { //indent: 缩进长度
        if (indent < 0) indent = 0;
        char[] chars = new char[indent];
        Arrays.fill(chars, ' ');
        final String space = new String(chars);
        StringBuilder sb = new StringBuilder();
        sb.append("{\r\n");
        for (Entry<String> en : getStringEntrys()) {
            sb.append(space).append("    '").append(en.name).append("': '").append(en.value).append("',\r\n");
        }
        for (Entry<AnyValue> en : getAnyEntrys()) {
            sb.append(space).append("    '").append(en.name).append("': '").append(en.value.toString(indent + 4)).append("',\r\n");
        }
        sb.append(space).append('}');
        return sb.toString();
    }

    public abstract void forEach(BiConsumer<String, String> stringConsumer);

    public abstract void forEach(BiConsumer<String, String> stringConsumer, BiConsumer<String, AnyValue> anyConsumer);

    public abstract Entry<String>[] getStringEntrys();

    public abstract Entry<AnyValue>[] getAnyEntrys();

    public abstract String[] getNames();

    public abstract String[] getValues(String name);

    public abstract String[] getValues(String... names);

    public abstract AnyValue[] getAnyValues(String name);

    public abstract AnyValue[] getAnyValues(String... names);

    public abstract AnyValue getAnyValue(String name);

    public abstract String getValue(String name);

    public boolean getBoolValue(String name) {
        return Boolean.parseBoolean(getValue(name));
    }

    public boolean getBoolValue(String name, boolean defaultValue) {
        String value = getValue(name);
        return value == null || value.length() == 0 ? defaultValue : Boolean.parseBoolean(value);
    }

    public byte getByteValue(String name) {
        return Byte.parseByte(getValue(name));
    }

    public byte getByteValue(String name, byte defaultValue) {
        String value = getValue(name);
        if (value == null || value.length() == 0) return defaultValue;
        try {
            return Byte.decode(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    public byte getByteValue(int radix, String name, byte defaultValue) {
        String value = getValue(name);
        if (value == null || value.length() == 0) return defaultValue;
        try {
            return (radix == 10 ? Byte.decode(value) : Byte.parseByte(value, radix));
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    public char getCharValue(String name) {
        return getValue(name).charAt(0);
    }

    public char getCharValue(String name, char defaultValue) {
        String value = getValue(name);
        return value == null || value.length() == 0 ? defaultValue : value.charAt(0);
    }

    public short getShortValue(String name) {
        return Short.decode(getValue(name));
    }

    public short getShortValue(String name, short defaultValue) {
        String value = getValue(name);
        if (value == null || value.length() == 0) return defaultValue;
        try {
            return Short.decode(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    public short getShortValue(int radix, String name, short defaultValue) {
        String value = getValue(name);
        if (value == null || value.length() == 0) return defaultValue;
        try {
            return (radix == 10 ? Short.decode(value) : Short.parseShort(value, radix));
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    public int getIntValue(String name) {
        return Integer.decode(getValue(name));
    }

    public int getIntValue(String name, int defaultValue) {
        String value = getValue(name);
        if (value == null || value.length() == 0) return defaultValue;
        try {
            return Integer.decode(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    public int getIntValue(int radix, String name, int defaultValue) {
        String value = getValue(name);
        if (value == null || value.length() == 0) return defaultValue;
        try {
            return (radix == 10 ? Integer.decode(value) : Integer.parseInt(value, radix));
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    public long getLongValue(String name) {
        return Long.decode(getValue(name));
    }

    public long getLongValue(String name, long defaultValue) {
        String value = getValue(name);
        if (value == null || value.length() == 0) return defaultValue;
        try {
            return Long.decode(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    public long getLongValue(int radix, String name, long defaultValue) {
        String value = getValue(name);
        if (value == null || value.length() == 0) return defaultValue;
        try {
            return (radix == 10 ? Long.decode(value) : Long.parseLong(value, radix));
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    public float getFloatValue(String name) {
        return Float.parseFloat(getValue(name));
    }

    public float getFloatValue(String name, float defaultValue) {
        String value = getValue(name);
        if (value == null || value.length() == 0) return defaultValue;
        try {
            return Float.parseFloat(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    public double getDoubleValue(String name) {
        return Double.parseDouble(getValue(name));
    }

    public double getDoubleValue(String name, double defaultValue) {
        String value = getValue(name);
        if (value == null || value.length() == 0) return defaultValue;
        try {
            return Double.parseDouble(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    public String getValue(String name, String defaultValue) {
        String value = getValue(name);
        return value == null ? defaultValue : value;
    }

    @Override
    public boolean equals(Object other) {
        if (!(other instanceof AnyValue)) return false;
        AnyValue conf = (AnyValue) other;
        if (!equals(this.getStringEntrys(), conf.getStringEntrys())) return false;
        return equals(this.getAnyEntrys(), conf.getAnyEntrys());
    }

    private static <T> boolean equals(Entry<? extends T>[] entry1, Entry<T>[] entry2) {
        if ((entry1 == null || entry1.length == 0) && (entry2 == null || entry2.length == 0)) return true;
        if (entry1.length != entry2.length) return false;
        for (int i = 0; i < entry1.length; i++) {
            if (!entry1[i].name.equals(entry2[i].name)) return false;
            if (!entry1[i].getValue().equals(entry2[i].getValue())) return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 19 * hash + Arrays.deepHashCode(this.getStringEntrys());
        hash = 19 * hash + Arrays.deepHashCode(this.getAnyEntrys());
        return hash;
    }

    public String toXML(String rootName) {
        return toXMLString(new StringBuilder("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n\r\n"), rootName, this, 0).toString();
    }

    protected static StringBuilder toXMLString(StringBuilder sb, String nodeName, AnyValue conf, int indent) { //indent: 缩进长度
        if (indent < 0) indent = 0;
        char[] chars = new char[indent];
        Arrays.fill(chars, ' ');
        final String space = new String(chars);
        Entry<AnyValue>[] anys = conf.getAnyEntrys();
        sb.append(space).append('<').append(nodeName);
        for (Entry<String> en : conf.getStringEntrys()) {
            sb.append(' ').append(en.name).append("=\"").append(en.value).append("\"");
        }
        if (anys == null || anys.length == 0) return sb.append("/>\r\n\r\n");
        sb.append(">\r\n\r\n");
        for (Entry<AnyValue> en : conf.getAnyEntrys()) {
            toXMLString(sb, en.name, en.getValue(), indent + 4);
        }
        return sb.append(space).append("</").append(nodeName).append(">\r\n\r\n");
    }

}
