package cn.taketoday.benchmark.http;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import cn.taketoday.benchmark.model.World;

public class WorldCache {

  private final World[] cache;

  public WorldCache(List<World> worlds) {
    this.cache = worlds.toArray(new World[0]);
  }

  public List<World> getCachedWorld(int count) {
    World[] worlds = this.cache;
    int length = worlds.length;
    ArrayList<World> ret = new ArrayList<>(count);
    ThreadLocalRandom current = ThreadLocalRandom.current();
    for (int i = 0; i < count; i++) {
      ret.add(worlds[current.nextInt(length - 1)]);
    }
    return ret;
  }

}