/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkale.source;

import java.io.*;
import java.lang.reflect.Type;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;
import java.util.logging.*;
import javax.annotation.Resource;
import org.redkale.convert.json.*;
import org.redkale.net.sncp.*;
import org.redkale.service.*;
import org.redkale.util.*;

/**
 * CacheSource的默认实现--内存缓存
 *
 * @param <V> value类型
 * <p>
 * 详情见: https://redkale.org
 *
 * @author zhangjx
 */
@SuppressWarnings("unchecked")
@Local
@AutoLoad(false)
@ResourceType(CacheSource.class)
public class CacheMemorySource<V extends Object> extends AbstractService implements CacheSource<V>, Service, AutoCloseable, Resourcable {

    private static final Type STRING_ENTRY_TYPE = new TypeToken<CacheEntry<String>>() {
    }.getType();

    private static final Type LONG_ENTRY_TYPE = new TypeToken<CacheEntry<Long>>() {
    }.getType();

    private static final Type ATOMIC_ENTRY_TYPE = new TypeToken<CacheEntry<AtomicLong>>() {
    }.getType();

    @Resource(name = "APP_HOME")
    private File home;

    @Resource
    private JsonConvert defaultConvert;

    @Resource(name = "$_convert")
    private JsonConvert convert;

    private boolean needStore;

    private Type objValueType;

    private ScheduledThreadPoolExecutor scheduler;

    private Consumer<CacheEntry> expireHandler;

    private final Logger logger = Logger.getLogger(this.getClass().getSimpleName());

    protected final ConcurrentHashMap<String, CacheEntry<Object>> container = new ConcurrentHashMap<>();

    @RpcRemote
    protected CacheSource<V> remoteSource;

    public CacheMemorySource() {
    }

    @Override
    public final void initValueType(Type valueType) {
        this.objValueType = valueType;
        this.initTransient(this.objValueType == null);
    }

    @Override
    public final void initTransient(boolean flag) {
        this.needStore = !flag;
    }

    @Override
    public final String getType() {
        return "memory";
    }

    @Override
    @SuppressWarnings("unchecked")
    public void init(AnyValue conf) {
        if (this.convert == null) this.convert = this.defaultConvert;
        if (this.convert == null) this.convert = JsonConvert.root();
        final CacheMemorySource self = this;
        AnyValue prop = conf == null ? null : conf.getAnyValue("properties");
        if (prop != null) {
            String storeValueStr = prop.getValue("value-type");
            if (storeValueStr != null) {
                try {
                    this.initValueType(Thread.currentThread().getContextClassLoader().loadClass(storeValueStr));
                } catch (Throwable e) {
                    logger.log(Level.SEVERE, self.getClass().getSimpleName() + " load key & value store class (" + storeValueStr + ") error", e);
                }
            }
            this.initTransient(prop.getBoolValue("store-ignore", false));
        }
        String expireHandlerClass = prop == null ? null : prop.getValue("expirehandler");
        if (expireHandlerClass != null) {
            try {
                this.expireHandler = (Consumer<CacheEntry>) Thread.currentThread().getContextClassLoader().loadClass(expireHandlerClass).getDeclaredConstructor().newInstance();
            } catch (Throwable e) {
                logger.log(Level.SEVERE, self.getClass().getSimpleName() + " new expirehandler class (" + expireHandlerClass + ") instance error", e);
            }
        }
        if (scheduler == null) {
            this.scheduler = new ScheduledThreadPoolExecutor(1, (Runnable r) -> {
                final Thread t = new Thread(r, self.getClass().getSimpleName() + "-Expirer-Thread");
                t.setDaemon(true);
                return t;
            });
            final List<String> keys = new ArrayList<>();
            scheduler.scheduleWithFixedDelay(() -> {
                keys.clear();
                int now = (int) (System.currentTimeMillis() / 1000);
                container.forEach((k, x) -> {
                    if (x.expireSeconds > 0 && (now > (x.lastAccessed + x.expireSeconds))) {
                        keys.add(x.key);
                    }
                });
                for (String key : keys) {
                    CacheEntry entry = container.remove(key);
                    if (expireHandler != null && entry != null) expireHandler.accept(entry);
                }
            }, 10, 10, TimeUnit.SECONDS);
            if (logger.isLoggable(Level.FINEST)) logger.finest(self.getClass().getSimpleName() + ":" + self.resourceName() + " start schedule expire executor");
        }
        if (Sncp.isRemote(self)) return;

        boolean datasync = false; //是否从远程同步过数据
        //----------同步数据……-----------
        // TODO
        if (this.needStore) {
            try {
                File store = home == null ? new File("cache/" + resourceName()) : new File(home, "cache/" + resourceName());
                if (!store.isFile() || !store.canRead()) return;
                LineNumberReader reader = new LineNumberReader(new FileReader(store));
                if (this.objValueType == null) this.objValueType = Object.class;
                final Type storeObjType = TypeToken.createParameterizedType(null, CacheEntry.class, objValueType);

                String line;
                while ((line = reader.readLine()) != null) {
                    if (line.isEmpty()) continue;
                    Type convertType = storeObjType;
                    if (line.startsWith("{\"cacheType\":\"" + CacheEntryType.LONG)) {
                        convertType = LONG_ENTRY_TYPE;
                    } else if (line.startsWith("{\"cacheType\":\"" + CacheEntryType.STRING)) {
                        convertType = STRING_ENTRY_TYPE;
                    } else if (line.startsWith("{\"cacheType\":\"" + CacheEntryType.ATOMIC)) {
                        convertType = ATOMIC_ENTRY_TYPE;
                    }
                    CacheEntry<Object> entry = convert.convertFrom(convertType, line);
                    if (entry.isExpired()) continue;
                    if (datasync && container.containsKey(entry.key)) continue; //已经同步了
                    container.put(entry.key, entry);
                }
                reader.close();
                store.delete();
            } catch (Exception e) {
                logger.log(Level.SEVERE, CacheSource.class.getSimpleName() + "(" + resourceName() + ") load store file error ", e);
            }
        }
        if (remoteSource != null && !Sncp.isRemote(this)) {
            SncpClient client = Sncp.getSncpClient((Service) remoteSource);
            if (client != null && client.getRemoteGroupTransport() != null
                && client.getRemoteGroupTransport().getRemoteAddresses().length > 0) {
                super.runAsync(() -> {
                    try {
                        CompletableFuture<List<CacheEntry<Object>>> listFuture = remoteSource.queryListAsync();
                        listFuture.whenComplete((list, exp) -> {
                            if (exp != null) {
                                if (logger.isLoggable(Level.FINEST)) logger.log(Level.FINEST, CacheSource.class.getSimpleName() + "(" + resourceName() + ") queryListAsync error", exp);
                            } else {
                                for (CacheEntry<Object> entry : list) {
                                    container.put(entry.key, entry);
                                }
                            }
                        });
                    } catch (Exception e) {
                        if (logger.isLoggable(Level.FINEST)) logger.log(Level.FINEST, CacheSource.class.getSimpleName() + "(" + resourceName() + ") queryListAsync error, maybe remote node connot connect ", e);
                    }
                });
            }
        }
    }

    /**
     * public static void main(String[] args) throws Exception {
     * AnyValue.DefaultAnyValue conf = new AnyValue.DefaultAnyValue();
     * conf.addValue("node", new AnyValue.DefaultAnyValue().addValue("addr", "127.0.0.1").addValue("port", "6379"));
     *
     * CacheMemorySource source = new CacheMemorySource();
     * source.defaultConvert = JsonFactory.root().getConvert();
     * source.initValueType(String.class); //value用String类型
     * source.initTransient(false);
     * source.init(conf);
     *
     * System.out.println("------------------------------------");
     * source.remove("key1");
     * source.remove("key2");
     * source.remove("300");
     * source.set("key1", "value1");
     * source.setString("keystr1", "strvalue1");
     * source.setLong("keylong1", 333L);
     * source.set("300", "4000");
     * source.getAndRefresh("key1", 3500);
     * System.out.println("[有值] 300 GET : " + source.get("300"));
     * System.out.println("[有值] key1 GET : " + source.get("key1"));
     * System.out.println("[无值] key2 GET : " + source.get("key2"));
     * System.out.println("[有值] keylong1 GET : " + source.getLong("keylong1", 0L));
     * System.out.println("[有值] key1 EXISTS : " + source.exists("key1"));
     * System.out.println("[无值] key2 EXISTS : " + source.exists("key2"));
     *
     * source.remove("keys3");
     * source.appendListItem("keys3", "vals1");
     * source.appendListItem("keys3", "vals2");
     * System.out.println("-------- keys3 追加了两个值 --------");
     * System.out.println("[两值] keys3 VALUES : " + source.getCollection("keys3"));
     * System.out.println("[有值] keys3 EXISTS : " + source.exists("keys3"));
     * source.removeListItem("keys3", "vals1");
     * System.out.println("[一值] keys3 VALUES : " + source.getCollection("keys3"));
     * source.getCollectionAndRefresh("keys3", 3000);
     *
     * source.remove("sets3");
     * source.appendSetItem("sets3", "setvals1");
     * source.appendSetItem("sets3", "setvals2");
     * source.appendSetItem("sets3", "setvals1");
     * System.out.println("[两值] sets3 VALUES : " + source.getCollection("sets3"));
     * System.out.println("[有值] sets3 EXISTS : " + source.exists("sets3"));
     * source.removeSetItem("sets3", "setvals1");
     * System.out.println("[一值] sets3 VALUES : " + source.getCollection("sets3"));
     * System.out.println("sets3 大小 : " + source.getCollectionSize("sets3"));
     * System.out.println("all keys: " + source.queryKeys());
     * System.out.println("newnum 值 : " + source.incr("newnum"));
     * System.out.println("newnum 值 : " + source.decr("newnum"));
     * System.out.println("------------------------------------");
     * source.destroy(null);
     * source.init(null);
     * System.out.println("all keys: " + source.queryKeys());
     * System.out.println("[有值] keylong1 GET : " + source.getLong("keylong1", 0L));
     * }
     */
    @Override
    public void close() throws Exception {  //给Application 关闭时调用
        destroy(null);
    }

    @Override
    public String resourceName() {
        Resource res = this.getClass().getAnnotation(Resource.class);
        return res == null ? "cachememory" : res.name();
    }

    @Override
    public void destroy(AnyValue conf) {
        if (scheduler != null) scheduler.shutdownNow();
        if (!this.needStore || Sncp.isRemote(this) || container.isEmpty()) return;
        try {
            File store = new File(home, "cache/" + resourceName());
            store.getParentFile().mkdirs();
            PrintStream stream = new PrintStream(store, "UTF-8");
            final Type storeObjType = TypeToken.createParameterizedType(null, CacheEntry.class, objValueType);
            final Type storeSetType = TypeToken.createParameterizedType(null, CacheEntry.class, objValueType);
            final Type storeListType = TypeToken.createParameterizedType(null, CacheEntry.class, objValueType);
            Collection<CacheEntry<Object>> entrys = container.values();
            for (CacheEntry entry : entrys) {
                Type convertType = storeObjType;
                if (entry.cacheType == CacheEntryType.LONG) {
                    convertType = LONG_ENTRY_TYPE;
                } else if (entry.cacheType == CacheEntryType.STRING) {
                    convertType = STRING_ENTRY_TYPE;
                } else if (entry.cacheType == CacheEntryType.ATOMIC) {
                    convertType = ATOMIC_ENTRY_TYPE;
                } else if (entry.cacheType == CacheEntryType.OBJECT) {
                    convertType = storeObjType;
                } else if (entry.cacheType == CacheEntryType.LONG_LIST) {
                    convertType = LONG_ENTRY_TYPE;
                } else if (entry.cacheType == CacheEntryType.LONG_SET) {
                    convertType = LONG_ENTRY_TYPE;
                } else if (entry.cacheType == CacheEntryType.STRING_LIST) {
                    convertType = STRING_ENTRY_TYPE;
                } else if (entry.cacheType == CacheEntryType.STRING_SET) {
                    convertType = STRING_ENTRY_TYPE;
                } else if (entry.cacheType == CacheEntryType.OBJECT_LIST) {
                    convertType = storeListType;
                } else if (entry.cacheType == CacheEntryType.OBJECT_SET) {
                    convertType = storeSetType;
                }
                try {
                    stream.println(convert.convertTo(convertType, entry));
                } catch (Exception ee) {
                    System.err.println(storeSetType + "-----" + entry);
                    throw ee;
                }
            }
            container.clear();
            stream.close();
        } catch (Exception e) {
            logger.log(Level.SEVERE, CacheSource.class.getSimpleName() + "(" + resourceName() + ") store to file error ", e);
        }
    }

    @Override
    public boolean exists(String key) {
        if (key == null) return false;
        CacheEntry entry = container.get(key);
        if (entry == null) return false;
        return !entry.isExpired();
    }

    @Override
    public CompletableFuture<Boolean> existsAsync(final String key) {
        return CompletableFuture.supplyAsync(() -> exists(key), getExecutor());
    }

    @Override
    @SuppressWarnings("unchecked")
    public V get(String key) {
        if (key == null) return null;
        CacheEntry entry = container.get(key);
        if (entry == null || entry.isExpired()) return null;
        if (entry.isListCacheType()) return (V) (entry.listValue == null ? null : new ArrayList(entry.listValue));
        if (entry.isSetCacheType()) return (V) (entry.csetValue == null ? null : new HashSet(entry.csetValue));
        return (V) entry.objectValue;
    }

    @Override
    public String getString(String key) {
        if (key == null) return null;
        CacheEntry entry = container.get(key);
        if (entry == null || entry.isExpired()) return null;
        return (String) entry.objectValue;
    }

    @Override
    public long getLong(String key, long defValue) {
        if (key == null) return defValue;
        CacheEntry entry = container.get(key);
        if (entry == null || entry.isExpired()) return defValue;
        return entry.objectValue == null ? defValue : (entry.objectValue instanceof AtomicLong ? ((AtomicLong) entry.objectValue).get() : (Long) entry.objectValue);
    }

    @Override
    public CompletableFuture<V> getAsync(final String key) {
        return CompletableFuture.supplyAsync(() -> get(key), getExecutor());
    }

    @Override
    public CompletableFuture<String> getStringAsync(final String key) {
        return CompletableFuture.supplyAsync(() -> getString(key), getExecutor());
    }

    @Override
    public CompletableFuture<Long> getLongAsync(final String key, long defValue) {
        return CompletableFuture.supplyAsync(() -> getLong(key, defValue), getExecutor());
    }

    @Override
    @RpcMultiRun
    @SuppressWarnings("unchecked")
    public V getAndRefresh(String key, final int expireSeconds) {
        if (key == null) return null;
        CacheEntry entry = container.get(key);
        if (entry == null || entry.isExpired()) return null;
        entry.lastAccessed = (int) (System.currentTimeMillis() / 1000);
        entry.expireSeconds = expireSeconds;
        if (entry.isListCacheType()) return (V) (entry.listValue == null ? null : new ArrayList(entry.listValue));
        if (entry.isSetCacheType()) return (V) (entry.csetValue == null ? null : new HashSet(entry.csetValue));
        return (V) entry.objectValue;
    }

    @Override
    @RpcMultiRun
    @SuppressWarnings("unchecked")
    public String getStringAndRefresh(String key, final int expireSeconds) {
        if (key == null) return null;
        CacheEntry entry = container.get(key);
        if (entry == null || entry.isExpired()) return null;
        entry.lastAccessed = (int) (System.currentTimeMillis() / 1000);
        entry.expireSeconds = expireSeconds;
        return (String) entry.objectValue;
    }

    @Override
    @RpcMultiRun
    public long getLongAndRefresh(String key, final int expireSeconds, long defValue) {
        if (key == null) return defValue;
        CacheEntry entry = container.get(key);
        if (entry == null || entry.isExpired()) return defValue;
        entry.lastAccessed = (int) (System.currentTimeMillis() / 1000);
        entry.expireSeconds = expireSeconds;
        return entry.objectValue == null ? defValue : (entry.objectValue instanceof AtomicLong ? ((AtomicLong) entry.objectValue).get() : (Long) entry.objectValue);

    }

    @Override
    @RpcMultiRun
    public CompletableFuture<V> getAndRefreshAsync(final String key, final int expireSeconds) {
        return CompletableFuture.supplyAsync(() -> getAndRefresh(key, expireSeconds), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<String> getStringAndRefreshAsync(final String key, final int expireSeconds) {
        return CompletableFuture.supplyAsync(() -> getStringAndRefresh(key, expireSeconds), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Long> getLongAndRefreshAsync(final String key, final int expireSeconds, long defValue) {
        return CompletableFuture.supplyAsync(() -> getLongAndRefresh(key, expireSeconds, defValue), getExecutor());
    }

    @Override
    @RpcMultiRun
    public void refresh(String key, final int expireSeconds) {
        if (key == null) return;
        CacheEntry entry = container.get(key);
        if (entry == null) return;
        entry.lastAccessed = (int) (System.currentTimeMillis() / 1000);
        entry.expireSeconds = expireSeconds;
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> refreshAsync(final String key, final int expireSeconds) {
        return CompletableFuture.runAsync(() -> refresh(key, expireSeconds), getExecutor());
    }

    protected void set(CacheEntryType cacheType, String key, Object value) {
        if (key == null) return;
        CacheEntry entry = container.get(key);
        if (entry == null) {
            entry = new CacheEntry(cacheType, key, value, null, null);
            container.putIfAbsent(key, entry);
        } else {
            entry.expireSeconds = 0;
            entry.objectValue = value;
            entry.lastAccessed = (int) (System.currentTimeMillis() / 1000);
        }
    }

    @Override
    @RpcMultiRun
    public void set(String key, V value) {
        set(CacheEntryType.OBJECT, key, value);
    }

    @Override
    @RpcMultiRun
    public void setString(String key, String value) {
        set(CacheEntryType.STRING, key, value);
    }

    @Override
    @RpcMultiRun
    public void setLong(String key, long value) {
        set(CacheEntryType.LONG, key, value);
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> setAsync(String key, V value) {
        return CompletableFuture.runAsync(() -> set(key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> setStringAsync(String key, String value) {
        return CompletableFuture.runAsync(() -> setString(key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> setLongAsync(String key, long value) {
        return CompletableFuture.runAsync(() -> setLong(key, value), getExecutor());
    }

    protected void set(CacheEntryType cacheType, int expireSeconds, String key, Object value) {
        if (key == null) return;
        CacheEntry entry = container.get(key);
        if (entry == null) {
            entry = new CacheEntry(cacheType, expireSeconds, key, value, null, null);
            container.putIfAbsent(key, entry);
        } else {
            if (expireSeconds > 0) entry.expireSeconds = expireSeconds;
            entry.lastAccessed = (int) (System.currentTimeMillis() / 1000);
            entry.objectValue = value;
        }
    }

    @Override
    @RpcMultiRun
    public void set(int expireSeconds, String key, V value) {
        set(CacheEntryType.OBJECT, expireSeconds, key, value);
    }

    @Override
    @RpcMultiRun
    public void setString(int expireSeconds, String key, String value) {
        set(CacheEntryType.STRING, expireSeconds, key, value);
    }

    @Override
    @RpcMultiRun
    public void setLong(int expireSeconds, String key, long value) {
        set(CacheEntryType.LONG, expireSeconds, key, value);
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> setAsync(int expireSeconds, String key, V value) {
        return CompletableFuture.runAsync(() -> set(expireSeconds, key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> setStringAsync(int expireSeconds, String key, String value) {
        return CompletableFuture.runAsync(() -> setString(expireSeconds, key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> setLongAsync(int expireSeconds, String key, long value) {
        return CompletableFuture.runAsync(() -> setLong(expireSeconds, key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public void setExpireSeconds(String key, int expireSeconds) {
        if (key == null) return;
        CacheEntry entry = container.get(key);
        if (entry == null) return;
        entry.expireSeconds = expireSeconds;
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> setExpireSecondsAsync(final String key, final int expireSeconds) {
        return CompletableFuture.runAsync(() -> setExpireSeconds(key, expireSeconds), getExecutor());
    }

    @Override
    @RpcMultiRun
    public void remove(String key) {
        if (key == null) return;
        container.remove(key);
    }

    @Override
    @RpcMultiRun
    public long incr(final String key) {
        return incr(key, 1);
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Long> incrAsync(final String key) {
        return CompletableFuture.supplyAsync(() -> incr(key), getExecutor());
    }

    @Override
    @RpcMultiRun
    public long incr(final String key, long num) {
        CacheEntry entry = container.get(key);
        if (entry == null) {
            synchronized (container) {
                entry = container.get(key);
                if (entry == null) {
                    entry = new CacheEntry(CacheEntryType.ATOMIC, key, new AtomicLong(), null, null);
                    container.put(key, entry);
                }
            }
        }
        return ((AtomicLong) entry.objectValue).addAndGet(num);
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Long> incrAsync(final String key, long num) {
        return CompletableFuture.supplyAsync(() -> incr(key, num), getExecutor());
    }

    @Override
    @RpcMultiRun
    public long decr(final String key) {
        return incr(key, -1);
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Long> decrAsync(final String key) {
        return CompletableFuture.supplyAsync(() -> decr(key), getExecutor());
    }

    @Override
    @RpcMultiRun
    public long decr(final String key, long num) {
        return incr(key, -num);
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Long> decrAsync(final String key, long num) {
        return CompletableFuture.supplyAsync(() -> decr(key, num), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> removeAsync(final String key) {
        return CompletableFuture.runAsync(() -> remove(key), getExecutor());
    }

    @Override
    public Collection<V> getCollection(final String key) {
        return (Collection<V>) get(key);
    }

    @Override
    public Collection<String> getStringCollection(final String key) {
        return (Collection<String>) get(key);
    }

    @Override
    public Collection<Long> getLongCollection(final String key) {
        return (Collection<Long>) get(key);
    }

    @Override
    public CompletableFuture<Collection<V>> getCollectionAsync(final String key) {
        return CompletableFuture.supplyAsync(() -> getCollection(key), getExecutor());
    }

    @Override
    public CompletableFuture<Collection<String>> getStringCollectionAsync(final String key) {
        return CompletableFuture.supplyAsync(() -> getStringCollection(key), getExecutor());
    }

    @Override
    public CompletableFuture<Collection<Long>> getLongCollectionAsync(final String key) {
        return CompletableFuture.supplyAsync(() -> getLongCollection(key), getExecutor());
    }

    @Override
    public int getCollectionSize(final String key) {
        Collection<V> collection = (Collection<V>) get(key);
        return collection == null ? 0 : collection.size();
    }

    @Override
    public CompletableFuture<Integer> getCollectionSizeAsync(final String key) {
        return CompletableFuture.supplyAsync(() -> getCollectionSize(key), getExecutor());
    }

    @Override
    @RpcMultiRun
    public Collection<V> getCollectionAndRefresh(final String key, final int expireSeconds) {
        return (Collection<V>) getAndRefresh(key, expireSeconds);
    }

    @Override
    @RpcMultiRun
    public Collection<String> getStringCollectionAndRefresh(final String key, final int expireSeconds) {
        return (Collection<String>) getAndRefresh(key, expireSeconds);
    }

    @Override
    public boolean existsSetItem(final String key, final V value) {
        Collection<V> list = getCollection(key);
        return list != null && list.contains(value);
    }

    @Override
    public CompletableFuture<Boolean> existsSetItemAsync(final String key, final V value) {
        return CompletableFuture.supplyAsync(() -> existsSetItem(key, value), getExecutor());
    }

    @Override
    public boolean existsStringSetItem(final String key, final String value) {
        Collection<String> list = getStringCollection(key);
        return list != null && list.contains(value);
    }

    @Override
    public CompletableFuture<Boolean> existsStringSetItemAsync(final String key, final String value) {
        return CompletableFuture.supplyAsync(() -> existsStringSetItem(key, value), getExecutor());
    }

    @Override
    public boolean existsLongSetItem(final String key, final long value) {
        Collection<Long> list = getLongCollection(key);
        return list != null && list.contains(value);
    }

    @Override
    public CompletableFuture<Boolean> existsLongSetItemAsync(final String key, final long value) {
        return CompletableFuture.supplyAsync(() -> existsLongSetItem(key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public Collection<Long> getLongCollectionAndRefresh(final String key, final int expireSeconds) {
        return (Collection<Long>) getAndRefresh(key, expireSeconds);
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Collection<V>> getCollectionAndRefreshAsync(final String key, final int expireSeconds) {
        return CompletableFuture.supplyAsync(() -> getCollectionAndRefresh(key, expireSeconds), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Collection<String>> getStringCollectionAndRefreshAsync(final String key, final int expireSeconds) {
        return CompletableFuture.supplyAsync(() -> getStringCollectionAndRefresh(key, expireSeconds), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Collection<Long>> getLongCollectionAndRefreshAsync(final String key, final int expireSeconds) {
        return CompletableFuture.supplyAsync(() -> getLongCollectionAndRefresh(key, expireSeconds), getExecutor());
    }

    protected void appendListItem(CacheEntryType cacheType, String key, Object value) {
        if (key == null) return;
        CacheEntry entry = container.get(key);
        if (entry == null || !entry.isListCacheType() || entry.listValue == null) {
            ConcurrentLinkedQueue list = new ConcurrentLinkedQueue();
            entry = new CacheEntry(cacheType, key, null, null, list);
            CacheEntry old = container.putIfAbsent(key, entry);
            if (old != null) list = old.listValue;
            if (list != null) list.add(value);
        } else {
            entry.listValue.add(value);
        }
    }

    @Override
    @RpcMultiRun
    public void appendListItem(String key, V value) {
        appendListItem(CacheEntryType.OBJECT_LIST, key, value);
    }

    @Override
    @RpcMultiRun
    public void appendStringListItem(String key, String value) {
        appendListItem(CacheEntryType.STRING_LIST, key, value);
    }

    @Override
    @RpcMultiRun
    public void appendLongListItem(String key, long value) {
        appendListItem(CacheEntryType.LONG_LIST, key, value);
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> appendListItemAsync(final String key, final V value) {
        return CompletableFuture.runAsync(() -> appendListItem(key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> appendStringListItemAsync(final String key, final String value) {
        return CompletableFuture.runAsync(() -> appendStringListItem(key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> appendLongListItemAsync(final String key, final long value) {
        return CompletableFuture.runAsync(() -> appendLongListItem(key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public void removeListItem(String key, V value) {
        if (key == null) return;
        CacheEntry entry = container.get(key);
        if (entry == null || entry.listValue == null) return;
        entry.listValue.remove(value);
    }

    @Override
    @RpcMultiRun
    public void removeStringListItem(String key, String value) {
        if (key == null) return;
        CacheEntry entry = container.get(key);
        if (entry == null || entry.listValue == null) return;
        entry.listValue.remove(value);
    }

    @Override
    @RpcMultiRun
    public void removeLongListItem(String key, long value) {
        if (key == null) return;
        CacheEntry entry = container.get(key);
        if (entry == null || entry.listValue == null) return;
        entry.listValue.remove(value);
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> removeListItemAsync(final String key, final V value) {
        return CompletableFuture.runAsync(() -> removeListItem(key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> removeStringListItemAsync(final String key, final String value) {
        return CompletableFuture.runAsync(() -> removeStringListItem(key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> removeLongListItemAsync(final String key, final long value) {
        return CompletableFuture.runAsync(() -> removeLongListItem(key, value), getExecutor());
    }

    protected void appendSetItem(CacheEntryType cacheType, String key, Object value) {
        if (key == null) return;
        CacheEntry entry = container.get(key);
        if (entry == null || !entry.isSetCacheType() || entry.csetValue == null) {
            CopyOnWriteArraySet set = new CopyOnWriteArraySet();
            entry = new CacheEntry(cacheType, key, null, set, null);
            CacheEntry old = container.putIfAbsent(key, entry);
            if (old != null) set = old.csetValue;
            if (set != null) set.add(value);
        } else {
            entry.csetValue.add(value);
        }
    }

    @Override
    @RpcMultiRun
    public void appendSetItem(String key, V value) {
        appendSetItem(CacheEntryType.OBJECT_SET, key, value);
    }

    @Override
    @RpcMultiRun
    public void appendStringSetItem(String key, String value) {
        appendSetItem(CacheEntryType.OBJECT_SET, key, value);
    }

    @Override
    @RpcMultiRun
    public void appendLongSetItem(String key, long value) {
        appendSetItem(CacheEntryType.OBJECT_SET, key, value);
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> appendSetItemAsync(final String key, final V value) {
        return CompletableFuture.runAsync(() -> appendSetItem(key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> appendStringSetItemAsync(final String key, final String value) {
        return CompletableFuture.runAsync(() -> appendStringSetItem(key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> appendLongSetItemAsync(final String key, final long value) {
        return CompletableFuture.runAsync(() -> appendLongSetItem(key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public void removeSetItem(String key, V value) {
        if (key == null) return;
        CacheEntry entry = container.get(key);
        if (entry == null || entry.csetValue == null) return;
        entry.csetValue.remove(value);
    }

    @Override
    @RpcMultiRun
    public void removeStringSetItem(String key, String value) {
        if (key == null) return;
        CacheEntry entry = container.get(key);
        if (entry == null || entry.csetValue == null) return;
        entry.csetValue.remove(value);
    }

    @Override
    @RpcMultiRun
    public void removeLongSetItem(String key, long value) {
        if (key == null) return;
        CacheEntry entry = container.get(key);
        if (entry == null || entry.csetValue == null) return;
        entry.csetValue.remove(value);
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> removeSetItemAsync(final String key, final V value) {
        return CompletableFuture.runAsync(() -> removeSetItem(key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> removeStringSetItemAsync(final String key, final String value) {
        return CompletableFuture.runAsync(() -> removeStringSetItem(key, value), getExecutor());
    }

    @Override
    @RpcMultiRun
    public CompletableFuture<Void> removeLongSetItemAsync(final String key, final long value) {
        return CompletableFuture.runAsync(() -> removeLongSetItem(key, value), getExecutor());
    }

    @Override
    public List<String> queryKeys() {
        return new ArrayList<>(container.keySet());
    }

    @Override
    public int getKeySize() {
        return container.size();
    }

    @Override
    public CompletableFuture<List<CacheEntry<Object>>> queryListAsync() {
        return CompletableFuture.completedFuture(new ArrayList<>(container.values()));
    }

    @Override
    public List<CacheEntry< Object>> queryList() {
        return new ArrayList<>(container.values());
    }

    @Override
    public CompletableFuture<List<String>> queryKeysAsync() {
        return CompletableFuture.completedFuture(new ArrayList<>(container.keySet()));
    }

    @Override
    public CompletableFuture<Integer> getKeySizeAsync() {
        return CompletableFuture.completedFuture(container.size());
    }
}
