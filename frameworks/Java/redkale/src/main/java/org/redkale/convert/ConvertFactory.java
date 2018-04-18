/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.convert;

import java.io.File;
import java.lang.reflect.*;
import java.math.*;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.channels.CompletionHandler;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.*;
import java.util.regex.Pattern;
import java.util.stream.*;
import org.redkale.convert.ext.*;
import org.redkale.util.*;

/**
 * 序列化模块的工厂类，用于注册自定义的序列化类型，获取Convert
 *
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 * @param <R> Reader输入的子类
 * @param <W> Writer输出的子类
 */
@SuppressWarnings("unchecked")
public abstract class ConvertFactory<R extends Reader, W extends Writer> {

    private final ConvertFactory parent;

    protected Convert<R, W> convert;

    protected boolean tiny;

    private final Encodeable<W, ?> anyEncoder = new AnyEncoder(this);

    //-----------------------------------------------------------------------------------
    private final ConcurrentHashMap<Class, Creator> creators = new ConcurrentHashMap();

    private final ConcurrentHashMap<String, Class> entitys = new ConcurrentHashMap();

    private final ConcurrentHashMap<Type, Decodeable<R, ?>> decoders = new ConcurrentHashMap();

    private final ConcurrentHashMap<Type, Encodeable<W, ?>> encoders = new ConcurrentHashMap();

    private final ConcurrentHashMap<AccessibleObject, ConvertColumnEntry> columnEntrys = new ConcurrentHashMap();

    private final Set<Class> skipIgnores = new HashSet();

    //key:需要屏蔽的字段；value：排除的字段名
    private final ConcurrentHashMap<Class, Set<String>> ignoreAlls = new ConcurrentHashMap();

    private boolean skipAllIgnore = false;

    protected ConvertFactory(ConvertFactory<R, W> parent, boolean tiny) {
        this.tiny = tiny;
        this.parent = parent;
        if (parent == null) {
            //---------------------------------------------------------
            this.register(boolean.class, BoolSimpledCoder.instance);
            this.register(Boolean.class, BoolSimpledCoder.instance);

            this.register(byte.class, ByteSimpledCoder.instance);
            this.register(Byte.class, ByteSimpledCoder.instance);

            this.register(short.class, ShortSimpledCoder.instance);
            this.register(Short.class, ShortSimpledCoder.instance);

            this.register(char.class, CharSimpledCoder.instance);
            this.register(Character.class, CharSimpledCoder.instance);

            this.register(int.class, IntSimpledCoder.instance);
            this.register(Integer.class, IntSimpledCoder.instance);

            this.register(long.class, LongSimpledCoder.instance);
            this.register(Long.class, LongSimpledCoder.instance);

            this.register(float.class, FloatSimpledCoder.instance);
            this.register(Float.class, FloatSimpledCoder.instance);

            this.register(double.class, DoubleSimpledCoder.instance);
            this.register(Double.class, DoubleSimpledCoder.instance);

            this.register(Number.class, NumberSimpledCoder.instance);
            this.register(String.class, StringSimpledCoder.instance);
            this.register(CharSequence.class, CharSequenceSimpledCoder.instance);
            this.register(java.util.Date.class, DateSimpledCoder.instance);
            this.register(AtomicInteger.class, AtomicIntegerSimpledCoder.instance);
            this.register(AtomicLong.class, AtomicLongSimpledCoder.instance);
            this.register(BigInteger.class, BigIntegerSimpledCoder.instance);
            this.register(BigDecimal.class, BigDecimalSimpledCoder.instance);
            this.register(InetAddress.class, InetAddressSimpledCoder.instance);
            this.register(DLong.class, DLongSimpledCoder.instance);
            this.register(Class.class, TypeSimpledCoder.instance);
            this.register(InetSocketAddress.class, InetAddressSimpledCoder.InetSocketAddressSimpledCoder.instance);
            this.register(Pattern.class, PatternSimpledCoder.instance);
            this.register(File.class, FileSimpledCoder.instance);
            this.register(CompletionHandler.class, CompletionHandlerSimpledCoder.instance);
            this.register(URL.class, URLSimpledCoder.instance);
            this.register(URI.class, URISimpledCoder.instance);
            //---------------------------------------------------------
            this.register(ByteBuffer.class, ByteBufferSimpledCoder.instance);
            this.register(boolean[].class, BoolArraySimpledCoder.instance);
            this.register(byte[].class, ByteArraySimpledCoder.instance);
            this.register(short[].class, ShortArraySimpledCoder.instance);
            this.register(char[].class, CharArraySimpledCoder.instance);
            this.register(int[].class, IntArraySimpledCoder.instance);
            this.register(IntStream.class, IntArraySimpledCoder.IntStreamSimpledCoder.instance);
            this.register(long[].class, LongArraySimpledCoder.instance);
            this.register(LongStream.class, LongArraySimpledCoder.LongStreamSimpledCoder.instance);
            this.register(float[].class, FloatArraySimpledCoder.instance);
            this.register(double[].class, DoubleArraySimpledCoder.instance);
            this.register(DoubleStream.class, DoubleArraySimpledCoder.DoubleStreamSimpledCoder.instance);
            this.register(String[].class, StringArraySimpledCoder.instance);
            //---------------------------------------------------------
            this.register(AnyValue.class, Creator.create(AnyValue.DefaultAnyValue.class));
            this.register(HttpCookie.class, new Creator<HttpCookie>() {
                @Override
                @ConstructorParameters({"name", "value"})
                public HttpCookie create(Object... params) {
                    return new HttpCookie((String) params[0], (String) params[1]);
                }

            });
        }
    }

    public ConvertFactory parent() {
        return this.parent;
    }

    public abstract ConvertType getConvertType();

    public abstract boolean isReversible(); //是否可逆的

    public abstract ConvertFactory createChild();

    public abstract ConvertFactory createChild(boolean tiny);

    public Convert getConvert() {
        return convert;
    }

    public ConvertFactory tiny(boolean tiny) {
        this.tiny = tiny;
        return this;
    }

    public boolean isConvertDisabled(AccessibleObject element) {
        ConvertDisabled[] ccs = element.getAnnotationsByType(ConvertDisabled.class);
        if (ccs.length == 0 && element instanceof Method) {
            final Method method = (Method) element;
            String fieldName = readGetSetFieldName(method);
            if (fieldName != null) {
                try {
                    ccs = method.getDeclaringClass().getDeclaredField(fieldName).getAnnotationsByType(ConvertDisabled.class);
                } catch (Exception e) { //说明没有该字段，忽略异常
                }
            }
        }
        final ConvertType ct = this.getConvertType();
        for (ConvertDisabled ref : ccs) {
            if (ref.type().contains(ct)) return true;
        }
        return false;
    }

    public ConvertColumnEntry findRef(AccessibleObject element) {
        if (element == null) return null;
        ConvertColumnEntry en = this.columnEntrys.get(element);
        Set<String> onlyColumns = null;
        if (element instanceof Method) {
            onlyColumns = ignoreAlls.get(((Method) element).getDeclaringClass());
        } else if (element instanceof Field) {
            onlyColumns = ignoreAlls.get(((Field) element).getDeclaringClass());
        }
        if (en != null && onlyColumns == null) return en;
        final ConvertType ct = this.getConvertType();
        ConvertColumn[] ccs = element.getAnnotationsByType(ConvertColumn.class);
        String fieldName = null;
        if (ccs.length == 0 && element instanceof Method) {
            final Method method = (Method) element;
            fieldName = readGetSetFieldName(method);
            if (fieldName != null) {
                try {
                    ccs = method.getDeclaringClass().getDeclaredField(fieldName).getAnnotationsByType(ConvertColumn.class);
                } catch (Exception e) { //说明没有该字段，忽略异常
                }
            }
        }
        if (onlyColumns != null && fieldName == null) {
            if (element instanceof Method) {
                fieldName = readGetSetFieldName((Method) element);
            } else if (element instanceof Field) {
                fieldName = ((Field) element).getName();
            }
        }
        if (ccs.length == 0 && onlyColumns != null && fieldName != null) {
            if (!onlyColumns.contains(fieldName)) return new ConvertColumnEntry(fieldName, true);
        }
        for (ConvertColumn ref : ccs) {
            if (ref.type().contains(ct)) {
                if (onlyColumns != null && fieldName != null) {
                    String realName = ref.name().isEmpty() ? fieldName : ref.name();
                    if (!onlyColumns.contains(realName)) return new ConvertColumnEntry(realName, true);
                }
                ConvertColumnEntry entry = new ConvertColumnEntry(ref);
                if (skipAllIgnore) {
                    entry.setIgnore(false);
                    return entry;
                }
                if (skipIgnores.isEmpty()) return entry;
                if (skipIgnores.contains(((Member) element).getDeclaringClass())) entry.setIgnore(false);
                return entry;
            }
        }
        return null;
    }

    static String readGetSetFieldName(Method method) {
        if (method == null) return null;
        String fname = method.getName();
        if (!fname.startsWith("is") && !fname.startsWith("get") && !fname.startsWith("set")) return fname;
        fname = fname.substring(fname.startsWith("is") ? 2 : 3);
        if (fname.length() > 1 && !(fname.charAt(1) >= 'A' && fname.charAt(1) <= 'Z')) {
            fname = Character.toLowerCase(fname.charAt(0)) + fname.substring(1);
        } else if (fname.length() == 1) {
            fname = "" + Character.toLowerCase(fname.charAt(0));
        }
        return fname;
    }

    final String getEntityAlias(Class clazz) {
        if (clazz == String.class) return "A";
        if (clazz == int.class) return "I";
        if (clazz == Integer.class) return "i";
        if (clazz == long.class) return "J";
        if (clazz == Long.class) return "j";
        if (clazz == byte.class) return "B";
        if (clazz == Byte.class) return "b";
        if (clazz == boolean.class) return "Z";
        if (clazz == Boolean.class) return "z";
        if (clazz == short.class) return "S";
        if (clazz == Short.class) return "s";
        if (clazz == char.class) return "C";
        if (clazz == Character.class) return "c";
        if (clazz == float.class) return "F";
        if (clazz == Float.class) return "f";
        if (clazz == double.class) return "D";
        if (clazz == Double.class) return "d";

        if (clazz == String[].class) return "[A";
        if (clazz == int[].class) return "[I";
        if (clazz == long[].class) return "[J";
        if (clazz == byte[].class) return "[B";
        if (clazz == boolean[].class) return "[Z";
        if (clazz == short[].class) return "[S";
        if (clazz == char[].class) return "[C";
        if (clazz == float[].class) return "[F";
        if (clazz == double[].class) return "[D";

        ConvertEntity ce = (ConvertEntity) clazz.getAnnotation(ConvertEntity.class);
        if (ce != null && findEntityAlias(ce.value()) == null) entitys.put(ce.value(), clazz);
        return ce == null ? clazz.getName() : ce.value();
    }

    final Class getEntityAlias(String name) {
        if ("A".equals(name)) return String.class;
        if ("I".equals(name)) return int.class;
        if ("i".equals(name)) return Integer.class;
        if ("J".equals(name)) return long.class;
        if ("j".equals(name)) return Long.class;
        if ("B".equals(name)) return byte.class;
        if ("b".equals(name)) return Byte.class;
        if ("Z".equals(name)) return boolean.class;
        if ("z".equals(name)) return Boolean.class;
        if ("S".equals(name)) return short.class;
        if ("s".equals(name)) return Short.class;
        if ("C".equals(name)) return char.class;
        if ("c".equals(name)) return Character.class;
        if ("F".equals(name)) return float.class;
        if ("f".equals(name)) return Float.class;
        if ("D".equals(name)) return double.class;
        if ("d".equals(name)) return Double.class;

        if ("[A".equals(name)) return String[].class;
        if ("[I".equals(name)) return int[].class;
        if ("[J".equals(name)) return long[].class;
        if ("[B".equals(name)) return byte[].class;
        if ("[Z".equals(name)) return boolean[].class;
        if ("[S".equals(name)) return short[].class;
        if ("[C".equals(name)) return char[].class;
        if ("[F".equals(name)) return float[].class;
        if ("[D".equals(name)) return double[].class;

        Class clazz = findEntityAlias(name);
        try {
            return clazz == null ? Thread.currentThread().getContextClassLoader().loadClass(name) : clazz;
        } catch (Exception ex) {
            throw new ConvertException("convert entity is " + name, ex);
        }
    }

    private Class findEntityAlias(String name) {
        Class clazz = entitys.get(name);
        return parent == null ? clazz : parent.findEntityAlias(name);
    }

    /**
     * 使所有类的所有被声明为ConvertColumn.ignore = true 的字段或方法变为ConvertColumn.ignore = false
     *
     * @param skipIgnore 是否忽略Ignore注解
     */
    public final void registerSkipAllIgnore(final boolean skipIgnore) {
        this.skipAllIgnore = skipIgnore;
    }

    /**
     * 使所有类的所有被声明为ConvertColumn.ignore = true 的字段或方法变为ConvertColumn.ignore = false
     *
     * @param skipIgnore 忽略ignore
     *
     * @return 自身
     */
    public ConvertFactory<R, W> skipAllIgnore(final boolean skipIgnore) {
        this.skipAllIgnore = skipIgnore;
        return this;
    }

    /**
     * 使该类所有被声明为ConvertColumn.ignore = true 的字段或方法变为ConvertColumn.ignore = false
     *
     * @param type 指定的类
     */
    public final void registerSkipIgnore(final Class type) {
        skipIgnores.add(type);
    }

    /**
     * 屏蔽指定类所有字段，仅仅保留指定字段 <br>
     * <b>注意: 该配置优先级高于skipAllIgnore和ConvertColumnEntry配置</b>
     *
     * @param type           指定的类
     * @param excludeColumns 需要排除的字段名
     */
    public final void registerIgnoreAll(final Class type, String... excludeColumns) {
        Set<String> set = ignoreAlls.get(type);
        if (set == null) {
            ignoreAlls.put(type, new HashSet<>(Arrays.asList(excludeColumns)));
        } else {
            set.addAll(Arrays.asList(excludeColumns));
        }
    }

    public final void register(final Class type, boolean ignore, String... columns) {
        for (String column : columns) {
            register(type, column, new ConvertColumnEntry(column, ignore));
        }
    }

    public final boolean register(final Class type, String column, String alias) {
        return register(type, column, new ConvertColumnEntry(alias));
    }

    public final boolean register(final Class type, String column, ConvertColumnEntry entry) {
        if (type == null || column == null || entry == null) return false;
        try {
            final Field field = type.getDeclaredField(column);
            String get = "get";
            if (field.getType() == boolean.class || field.getType() == Boolean.class) get = "is";
            char[] cols = column.toCharArray();
            cols[0] = Character.toUpperCase(cols[0]);
            String col2 = new String(cols);
            try {
                register(type.getMethod(get + col2), entry);
            } catch (Exception ex) {
            }
            try {
                register(type.getMethod("set" + col2, field.getType()), entry);
            } catch (Exception ex) {
            }
            return register(field, entry);
        } catch (Exception e) {
            return false;
        }
    }

    public final <E> boolean register(final AccessibleObject field, final ConvertColumnEntry entry) {
        if (field == null || entry == null) return false;
        this.columnEntrys.put(field, entry);
        return true;
    }

    public final void reloadCoder(final Type type) {
        this.register(type, this.createDecoder(type));
        this.register(type, this.createEncoder(type));
    }

    public final void reloadCoder(final Type type, final Class clazz) {
        this.register(type, this.createDecoder(type, clazz));
        this.register(type, this.createEncoder(type, clazz));
    }

    public final <E> void register(final Class<E> clazz, final Creator<? extends E> creator) {
        creators.put(clazz, creator);
    }

    public final <T> Creator<T> findCreator(Class<T> type) {
        Creator<T> creator = creators.get(type);
        if (creator != null) return creator;
        return this.parent == null ? null : this.parent.findCreator(type);
    }

    public final <T> Creator<T> loadCreator(Class<T> type) {
        Creator result = findCreator(type);
        if (result == null) {
            result = Creator.create(type);
            if (result != null) creators.put(type, result);
        }
        return result;
    }

    //----------------------------------------------------------------------
    public final <E> Encodeable<W, E> getAnyEncoder() {
        return (Encodeable<W, E>) anyEncoder;
    }

    public final <E> void register(final Type clazz, final SimpledCoder<R, W, E> coder) {
        decoders.put(clazz, coder);
        encoders.put(clazz, coder);
    }

    public final <E> void register(final Type clazz, final Decodeable<R, E> decoder, final Encodeable<W, E> encoder) {
        decoders.put(clazz, decoder);
        encoders.put(clazz, encoder);
    }

    public final <E> void register(final Type clazz, final Decodeable<R, E> decoder) {
        decoders.put(clazz, decoder);
    }

    public final <E> void register(final Type clazz, final Encodeable<W, E> encoder) {
        encoders.put(clazz, encoder);
    }

    public final <E> Decodeable<R, E> findDecoder(final Type type) {
        Decodeable<R, E> rs = (Decodeable<R, E>) decoders.get(type);
        if (rs != null) return rs;
        return this.parent == null ? null : this.parent.findDecoder(type);
    }

    public final <E> Encodeable<W, E> findEncoder(final Type type) {
        Encodeable<W, E> rs = (Encodeable<W, E>) encoders.get(type);
        if (rs != null) return rs;
        return this.parent == null ? null : this.parent.findEncoder(type);
    }

    public final <E> Decodeable<R, E> loadDecoder(final Type type) {
        Decodeable<R, E> decoder = findDecoder(type);
        if (decoder != null) return decoder;
        if (type instanceof GenericArrayType) return new ArrayDecoder(this, type);
        Class clazz;
        if (type instanceof ParameterizedType) {
            final ParameterizedType pts = (ParameterizedType) type;
            clazz = (Class) (pts).getRawType();
        } else if (type instanceof TypeVariable) { // e.g.  <? extends E>
            final TypeVariable tv = (TypeVariable) type;
            Class cz = tv.getBounds().length == 0 ? Object.class : null;
            for (Type f : tv.getBounds()) {
                if (f instanceof Class) {
                    cz = (Class) f;
                    break;
                }
            }
            clazz = cz;
            if (cz == null) throw new ConvertException("not support the type (" + type + ")");
        } else if (type instanceof WildcardType) { // e.g. <? extends Serializable>
            final WildcardType wt = (WildcardType) type;
            Class cz = null;
            for (Type f : wt.getUpperBounds()) {
                if (f instanceof Class) {
                    cz = (Class) f;
                    break;
                }
            }
            clazz = cz;
            if (cz == null) throw new ConvertException("not support the type (" + type + ")");
        } else if (type instanceof Class) {
            clazz = (Class) type;
        } else {
            throw new ConvertException("not support the type (" + type + ")");
        }
        decoder = findDecoder(clazz);
        if (decoder != null) return decoder;
        return createDecoder(type, clazz);
    }

    public final <E> Decodeable<R, E> createDecoder(final Type type) {
        Class clazz;
        if (type instanceof ParameterizedType) {
            final ParameterizedType pts = (ParameterizedType) type;
            clazz = (Class) (pts).getRawType();
        } else if (type instanceof Class) {
            clazz = (Class) type;
        } else {
            throw new ConvertException("not support the type (" + type + ")");
        }
        return createDecoder(type, clazz);
    }

    private <E> Decodeable<R, E> createDecoder(final Type type, final Class clazz) {
        Decodeable<R, E> decoder = null;
        ObjectDecoder od = null;
        if (clazz.isEnum()) {
            decoder = new EnumSimpledCoder(clazz);
        } else if (clazz.isArray()) {
            decoder = new ArrayDecoder(this, type);
        } else if (Collection.class.isAssignableFrom(clazz)) {
            decoder = new CollectionDecoder(this, type);
        } else if (Stream.class.isAssignableFrom(clazz)) {
            decoder = new StreamDecoder(this, type);
        } else if (Map.class.isAssignableFrom(clazz)) {
            decoder = new MapDecoder(this, type);
        } else if (Optional.class == clazz) {
            decoder = new OptionalCoder(this, type);
        } else if (clazz == Object.class) {
            od = new ObjectDecoder(type);
            decoder = od;
        } else if (!clazz.getName().startsWith("java.")
            || java.net.HttpCookie.class == clazz
            || clazz.getName().startsWith("java.awt.geom.Point2D")) {
            Decodeable simpleCoder = null;
            for (final Method method : clazz.getDeclaredMethods()) {
                if (!Modifier.isStatic(method.getModifiers())) continue;
                Class[] paramTypes = method.getParameterTypes();
                if (paramTypes.length != 1) continue;
                if (paramTypes[0] != ConvertFactory.class && paramTypes[0] != this.getClass()) continue;
                if (!Decodeable.class.isAssignableFrom(method.getReturnType())) continue;
                try {
                    method.setAccessible(true);
                    simpleCoder = (Decodeable) method.invoke(null, this);
                    break;
                } catch (Exception e) {
                }
            }
            if (simpleCoder == null) {
                od = new ObjectDecoder(type);
                decoder = od;
            } else {
                decoder = simpleCoder;
            }
        }
        if (decoder == null) throw new ConvertException("not support the type (" + type + ")");
        register(type, decoder);
        if (od != null) od.init(this);
        return decoder;
    }

    public final <E> Encodeable<W, E> loadEncoder(final Type type) {
        Encodeable<W, E> encoder = findEncoder(type);
        if (encoder != null) return encoder;
        if (type instanceof GenericArrayType) return new ArrayEncoder(this, type);
        Class clazz;
        if (type instanceof ParameterizedType) {
            final ParameterizedType pts = (ParameterizedType) type;
            clazz = (Class) (pts).getRawType();
        } else if (type instanceof TypeVariable) {
            TypeVariable tv = (TypeVariable) type;
            Type t = Object.class;
            if (tv.getBounds().length == 1) {
                t = tv.getBounds()[0];
            }
            if (!(t instanceof Class)) t = Object.class;
            clazz = (Class) t;
        } else if (type instanceof Class) {
            clazz = (Class) type;
        } else {
            throw new ConvertException("not support the type (" + type + ")");
        }
        encoder = findEncoder(clazz);
        if (encoder != null) return encoder;
        return createEncoder(type, clazz);
    }

    public final <E> Encodeable<W, E> createEncoder(final Type type) {
        Class clazz;
        if (type instanceof ParameterizedType) {
            final ParameterizedType pts = (ParameterizedType) type;
            clazz = (Class) (pts).getRawType();
        } else if (type instanceof Class) {
            clazz = (Class) type;
        } else {
            throw new ConvertException("not support the type (" + type + ")");
        }
        return createEncoder(type, clazz);
    }

    private <E> Encodeable<W, E> createEncoder(final Type type, final Class clazz) {
        Encodeable<W, E> encoder = null;
        ObjectEncoder oe = null;
        if (clazz.isEnum()) {
            encoder = new EnumSimpledCoder(clazz);
        } else if (clazz.isArray()) {
            encoder = new ArrayEncoder(this, type);
        } else if (Collection.class.isAssignableFrom(clazz)) {
            encoder = new CollectionEncoder(this, type);
        } else if (Stream.class.isAssignableFrom(clazz)) {
            encoder = new StreamEncoder(this, type);
        } else if (Map.class.isAssignableFrom(clazz)) {
            encoder = new MapEncoder(this, type);
        } else if (Optional.class == clazz) {
            encoder = new OptionalCoder(this, type);
        } else if (clazz == Object.class) {
            return (Encodeable<W, E>) this.anyEncoder;
        } else if (!clazz.getName().startsWith("java.") || java.net.HttpCookie.class == clazz) {
            Encodeable simpleCoder = null;
            for (final Method method : clazz.getDeclaredMethods()) {
                if (!Modifier.isStatic(method.getModifiers())) continue;
                Class[] paramTypes = method.getParameterTypes();
                if (paramTypes.length != 1) continue;
                if (paramTypes[0] != ConvertFactory.class && paramTypes[0] != this.getClass()) continue;
                if (!Encodeable.class.isAssignableFrom(method.getReturnType())) continue;
                try {
                    method.setAccessible(true);
                    simpleCoder = (Encodeable) method.invoke(null, this);
                    break;
                } catch (Exception e) {
                }
            }
            if (simpleCoder == null) {
                oe = new ObjectEncoder(type);
                encoder = oe;
            } else {
                encoder = simpleCoder;
            }
        }
        if (encoder == null) throw new ConvertException("not support the type (" + type + ")");
        register(type, encoder);
        if (oe != null) oe.init(this);
        return encoder;

    }

}
