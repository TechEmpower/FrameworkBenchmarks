package net.officefloor.benchmark;

import java.util.HashMap;
import java.util.Map;

import net.officefloor.cache.Cache;
import net.officefloor.cache.constant.ConstantCacheDataRetriever;
import net.officefloor.plugin.clazz.Dependency;

/**
 * {@link ConstantCacheDataRetriever} containing polling logic to refresh the
 * {@link Cache}.
 * 
 * @author Daniel Sagenschneider
 */
public class CachedWorldDataRetriever implements ConstantCacheDataRetriever<Integer, CachedWorld> {

	private @Dependency WorldRepository repository;

	@Override
	public Map<Integer, CachedWorld> getData() throws Exception {
		Map<Integer, CachedWorld> data = new HashMap<>();
		for (World world : this.repository.findAll()) {
			data.put(world.getId(), new CachedWorld(world.getId(), world.getRandomNumber()));
		}
		return data;
	}

}