package net.officefloor.benchmark;

import java.sql.Connection;
import java.sql.ResultSet;
import java.util.HashMap;
import java.util.Map;

import javax.sql.DataSource;

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

	private @Dependency DataSource dataSource;

	@Override
	public Map<Integer, CachedWorld> getData() throws Exception {
		try (Connection connection = this.dataSource.getConnection()) {
			Map<Integer, CachedWorld> data = new HashMap<>();
			ResultSet resultSet = connection.prepareStatement("SELECT id, randomNumber FROM World")
					.executeQuery();
			while (resultSet.next()) {
				int id = resultSet.getInt("id");
				int randomNumber = resultSet.getInt("randomNumber");
				data.put(id, new CachedWorld(id, randomNumber));
			}
			return data;
		}
	}

}