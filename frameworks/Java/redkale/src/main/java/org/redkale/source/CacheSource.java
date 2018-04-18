/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.lang.reflect.Type;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.Function;
import org.redkale.convert.ConvertColumn;
import org.redkale.convert.json.JsonFactory;
import org.redkale.util.ConstructorParameters;

/**
 * Redkale中缓存数据源的核心类。 主要供业务开发者使用， 技术开发者提供CacheSource的实现。<br>
 * CacheSource提供三种数据类型操作: String、Long和泛型指定的数据类型。<br>
 * String统一用setString、getString等系列方法。<br>
 * Long统一用setLong、getLong、incr等系列方法。<br>
 * 其他则供自定义数据类型使用。
 *
 * @param <V> value的类型
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
public interface CacheSource<V extends Object> {

    public String getType();

    public void initValueType(Type valueType);

    public void initTransient(boolean flag);

    default boolean isOpen() {
        return true;
    }

    public boolean exists(final String key);

    public V get(final String key);

    default V getIfAbsent(final String key, Function<String, ? extends V> mappingFunction) {
        V rs = get(key);
        if (rs == null) {
            rs = mappingFunction.apply(key);
            if (rs != null) set(key, rs);
        }
        return rs;
    }

    public V getAndRefresh(final String key, final int expireSeconds);

    default V getAndRefreshIfAbsent(final String key, final int expireSeconds, Function<String, ? extends V> mappingFunction) {
        V rs = getAndRefresh(key, expireSeconds);
        if (rs == null) {
            rs = mappingFunction.apply(key);
            if (rs != null) set(expireSeconds, key, rs);
        }
        return rs;
    }

    public void refresh(final String key, final int expireSeconds);

    public void set(final String key, final V value);

    public void set(final int expireSeconds, final String key, final V value);

    public void setExpireSeconds(final String key, final int expireSeconds);

    public void remove(final String key);

    public long incr(final String key);

    public long incr(final String key, long num);

    public long decr(final String key);

    public long decr(final String key, long num);

    public Collection<V> getCollection(final String key);

    public int getCollectionSize(final String key);

    public Collection<V> getCollectionAndRefresh(final String key, final int expireSeconds);

    public void appendListItem(final String key, final V value);

    public void removeListItem(final String key, final V value);

    public boolean existsSetItem(final String key, final V value);

    public void appendSetItem(final String key, final V value);

    public void removeSetItem(final String key, final V value);

    public List<String> queryKeys();

    public int getKeySize();

    public List<CacheEntry<Object>> queryList();

    public String getString(final String key);

    public String getStringAndRefresh(final String key, final int expireSeconds);

    public void setString(final String key, final String value);

    public void setString(final int expireSeconds, final String key, final String value);

    public Collection<String> getStringCollection(final String key);

    public Collection<String> getStringCollectionAndRefresh(final String key, final int expireSeconds);

    public void appendStringListItem(final String key, final String value);

    public void removeStringListItem(final String key, final String value);

    public boolean existsStringSetItem(final String key, final String value);

    public void appendStringSetItem(final String key, final String value);

    public void removeStringSetItem(final String key, final String value);

    public long getLong(final String key, long defValue);

    public long getLongAndRefresh(final String key, final int expireSeconds, long defValue);

    public void setLong(final String key, final long value);

    public void setLong(final int expireSeconds, final String key, final long value);

    public Collection<Long> getLongCollection(final String key);

    public Collection<Long> getLongCollectionAndRefresh(final String key, final int expireSeconds);

    public void appendLongListItem(final String key, final long value);

    public void removeLongListItem(final String key, final long value);

    public boolean existsLongSetItem(final String key, final long value);

    public void appendLongSetItem(final String key, final long value);

    public void removeLongSetItem(final String key, final long value);

    //---------------------- CompletableFuture 异步版 ---------------------------------
    public CompletableFuture<Boolean> existsAsync(final String key);

    public CompletableFuture<V> getAsync(final String key);

    default CompletableFuture<V> getIfAbsentAsync(final String key, Function<String, ? extends V> mappingFunction) {
        return getAsync(key).thenCompose((V rs) -> {
            if (rs == null) {
                rs = mappingFunction.apply(key);
                if (rs != null) {
                    final V v = rs;
                    return setAsync(key, rs).thenApply((k) -> v);
                }
            }
            return CompletableFuture.completedFuture(rs);
        });
    }

    public CompletableFuture<V> getAndRefreshAsync(final String key, final int expireSeconds);

    default CompletableFuture<V> getAndRefreshIfAbsentAsync(final String key, final int expireSeconds, Function<String, ? extends V> mappingFunction) {
        return getAndRefreshAsync(key, expireSeconds).thenCompose((V rs) -> {
            if (rs == null) {
                rs = mappingFunction.apply(key);
                if (rs != null) {
                    final V v = rs;
                    return setAsync(expireSeconds, key, rs).thenApply((k) -> v);
                }
            }
            return CompletableFuture.completedFuture(rs);
        });
    }

    public CompletableFuture<Void> refreshAsync(final String key, final int expireSeconds);

    public CompletableFuture<Void> setAsync(final String key, final V value);

    public CompletableFuture<Void> setAsync(final int expireSeconds, final String key, final V value);

    public CompletableFuture<Void> setExpireSecondsAsync(final String key, final int expireSeconds);

    public CompletableFuture<Void> removeAsync(final String key);

    public CompletableFuture<Long> incrAsync(final String key);

    public CompletableFuture<Long> incrAsync(final String key, long num);

    public CompletableFuture<Long> decrAsync(final String key);

    public CompletableFuture<Long> decrAsync(final String key, long num);

    public CompletableFuture<Collection<V>> getCollectionAsync(final String key);

    public CompletableFuture<Integer> getCollectionSizeAsync(final String key);

    public CompletableFuture<Collection<V>> getCollectionAndRefreshAsync(final String key, final int expireSeconds);

    public CompletableFuture<Void> appendListItemAsync(final String key, final V value);

    public CompletableFuture<Void> removeListItemAsync(final String key, final V value);

    public CompletableFuture<Boolean> existsSetItemAsync(final String key, final V value);

    public CompletableFuture<Void> appendSetItemAsync(final String key, final V value);

    public CompletableFuture<Void> removeSetItemAsync(final String key, final V value);

    public CompletableFuture<List<String>> queryKeysAsync();

    public CompletableFuture<Integer> getKeySizeAsync();

    public CompletableFuture<List<CacheEntry< Object>>> queryListAsync();

    public CompletableFuture<String> getStringAsync(final String key);

    public CompletableFuture<String> getStringAndRefreshAsync(final String key, final int expireSeconds);

    public CompletableFuture<Void> setStringAsync(final String key, final String value);

    public CompletableFuture<Void> setStringAsync(final int expireSeconds, final String key, final String value);

    public CompletableFuture<Collection<String>> getStringCollectionAsync(final String key);

    public CompletableFuture<Collection<String>> getStringCollectionAndRefreshAsync(final String key, final int expireSeconds);

    public CompletableFuture<Void> appendStringListItemAsync(final String key, final String value);

    public CompletableFuture<Void> removeStringListItemAsync(final String key, final String value);

    public CompletableFuture<Boolean> existsStringSetItemAsync(final String key, final String value);

    public CompletableFuture<Void> appendStringSetItemAsync(final String key, final String value);

    public CompletableFuture<Void> removeStringSetItemAsync(final String key, final String value);

    public CompletableFuture<Long> getLongAsync(final String key, long defValue);

    public CompletableFuture<Long> getLongAndRefreshAsync(final String key, final int expireSeconds, long defValue);

    public CompletableFuture<Void> setLongAsync(final String key, long value);

    public CompletableFuture<Void> setLongAsync(final int expireSeconds, final String key, final long value);

    public CompletableFuture<Collection<Long>> getLongCollectionAsync(final String key);

    public CompletableFuture<Collection<Long>> getLongCollectionAndRefreshAsync(final String key, final int expireSeconds);

    public CompletableFuture<Void> appendLongListItemAsync(final String key, final long value);

    public CompletableFuture<Void> removeLongListItemAsync(final String key, final long value);

    public CompletableFuture<Boolean> existsLongSetItemAsync(final String key, final long value);

    public CompletableFuture<Void> appendLongSetItemAsync(final String key, final long value);

    public CompletableFuture<Void> removeLongSetItemAsync(final String key, final long value);

    default CompletableFuture<Boolean> isOpenAsync() {
        return CompletableFuture.completedFuture(isOpen());
    }

    public static enum CacheEntryType {
        LONG, STRING, OBJECT, ATOMIC,
        LONG_SET, STRING_SET, OBJECT_SET,
        LONG_LIST, STRING_LIST, OBJECT_LIST;
    }

    public static final class CacheEntry<T> {

        final CacheEntryType cacheType;

        final String key;

        //<=0表示永久保存
        int expireSeconds;

        volatile int lastAccessed; //最后刷新时间

        T objectValue;

        CopyOnWriteArraySet<T> csetValue;

        ConcurrentLinkedQueue<T> listValue;

        public CacheEntry(CacheEntryType cacheType, String key, T objectValue, CopyOnWriteArraySet<T> csetValue, ConcurrentLinkedQueue<T> listValue) {
            this(cacheType, 0, key, objectValue, csetValue, listValue);
        }

        public CacheEntry(CacheEntryType cacheType, int expireSeconds, String key, T objectValue, CopyOnWriteArraySet<T> csetValue, ConcurrentLinkedQueue<T> listValue) {
            this(cacheType, expireSeconds, (int) (System.currentTimeMillis() / 1000), key, objectValue, csetValue, listValue);
        }

        @ConstructorParameters({"cacheType", "expireSeconds", "lastAccessed", "key", "objectValue", "csetValue", "listValue"})
        public CacheEntry(CacheEntryType cacheType, int expireSeconds, int lastAccessed, String key, T objectValue, CopyOnWriteArraySet<T> csetValue, ConcurrentLinkedQueue<T> listValue) {
            this.cacheType = cacheType;
            this.expireSeconds = expireSeconds;
            this.lastAccessed = lastAccessed;
            this.key = key;
            this.objectValue = objectValue;
            this.csetValue = csetValue;
            this.listValue = listValue;
        }

        @Override
        public String toString() {
            return JsonFactory.root().getConvert().convertTo(this);
        }

        @ConvertColumn(ignore = true)
        public boolean isListCacheType() {
            return cacheType == CacheEntryType.LONG_LIST || cacheType == CacheEntryType.STRING_LIST || cacheType == CacheEntryType.OBJECT_LIST;
        }

        @ConvertColumn(ignore = true)
        public boolean isSetCacheType() {
            return cacheType == CacheEntryType.LONG_SET || cacheType == CacheEntryType.STRING_SET || cacheType == CacheEntryType.OBJECT_SET;
        }

        @ConvertColumn(ignore = true)
        public boolean isExpired() {
            return (expireSeconds > 0 && lastAccessed + expireSeconds < (System.currentTimeMillis() / 1000));
        }

        public CacheEntryType getCacheType() {
            return cacheType;
        }

        public int getExpireSeconds() {
            return expireSeconds;
        }

        public int getLastAccessed() {
            return lastAccessed;
        }

        public String getKey() {
            return key;
        }

        public T getObjectValue() {
            return objectValue;
        }

        public CopyOnWriteArraySet<T> getCsetValue() {
            return csetValue;
        }

        public ConcurrentLinkedQueue<T> getListValue() {
            return listValue;
        }

    }
}
