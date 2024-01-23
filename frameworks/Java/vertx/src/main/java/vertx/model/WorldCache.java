package vertx.model;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

public class WorldCache {

    private Cache<Integer, CachedWorld> cache;

    public WorldCache(List<CachedWorld> worlds) {
        Cache<Integer, CachedWorld> cache = Caffeine.newBuilder().maximumSize(10_000).build();
        int key = 0;
        for (CachedWorld world : worlds) {
            cache.put(key++, world);
        }
        this.cache = cache;
    }

    public CachedWorld[] getCachedWorld(int count) {
        CachedWorld[] ret = new CachedWorld[count];
        ThreadLocalRandom current = ThreadLocalRandom.current();
        for (int i = 0;i < count;i++) {
            Integer key = Integer.valueOf(current.nextInt(1000));
            ret[i] = cache.getIfPresent(key);
        }
        return ret;
    }
}
