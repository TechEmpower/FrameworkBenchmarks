package com.techempower.benchmark.pippo.dao;

import com.techempower.benchmark.pippo.BenchmarkEnvironment;
import com.techempower.benchmark.pippo.BenchmarkUtils;
import com.techempower.benchmark.pippo.model.Fortune;
import com.techempower.benchmark.pippo.model.World;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import javax.sql.DataSource;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class SqlDao implements Dao {

	public SqlDao(String database) {
		this.database = database;
	}

	@Override
	public World getRandomWorld() {
		try (
			Connection connection = getDataSource().getConnection();
			PreparedStatement statement = connection.prepareStatement(SelectRandomWorld)
		) {
			statement.setInt(1, BenchmarkUtils.random());
			try (ResultSet resultSet = statement.executeQuery()) {
				resultSet.next();
				int id = resultSet.getInt(1);
				int randomNumber = resultSet.getInt(2);
				return new World(id, randomNumber);
			}
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<World> getRandomWorlds(int count) {
		List<World> worlds = new ArrayList<>();
		try (
			Connection connection = getDataSource().getConnection();
			PreparedStatement statement = connection.prepareStatement(SelectRandomWorld)
		) {
			for (int i = 0; i < count; i++) {
				statement.setInt(1, BenchmarkUtils.random());
				try (ResultSet resultSet = statement.executeQuery()) {
					resultSet.next();
					int id = resultSet.getInt(1);
					int randomNumber = resultSet.getInt(2);
					worlds.add(new World(id, randomNumber));
				}
			}
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
		return worlds;
	}

	@Override
	public void updateRandomWorlds(List<World> worlds) {
		try (
			Connection connection = getDataSource().getConnection();
			PreparedStatement statement = connection.prepareStatement(UpdateRandomWorld)
		) {
			connection.setAutoCommit(false);
			// sort to prevent deadlocks
			worlds.sort(Comparator.comparing(world -> world.id));
			for (World world : worlds) {
				statement.setInt(1, world.randomNumber);
				statement.setInt(2, world.id);
				statement.addBatch();
			}
			statement.executeBatch();
			connection.commit();
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public List<Fortune> getFortunes() {
		try (
			Connection connection = getDataSource().getConnection();
			PreparedStatement statement = connection.prepareStatement(SelectFortunes)
		) {
			List<Fortune> fortunes = new ArrayList<>();
			try (ResultSet resultSet = statement.executeQuery()) {
				while (resultSet.next()) {
					int id = resultSet.getInt(1);
					String message = resultSet.getString(2);
					fortunes.add(new Fortune(id, message));
				}
			}
			return fortunes;
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public void close() {
		if (dataSource != null)
			dataSource.close();
	}

	private DataSource getDataSource() {

		if (dataSource == null) {
			dataSourceLock.lock();
			try {
				if (dataSource == null) {
					HikariConfig config;
					try (InputStream istream = Thread.currentThread().getContextClassLoader().getResourceAsStream(String.format("hikari-%s.properties", database))) {
						Properties properties = new Properties();
						properties.load(istream);
						config = new HikariConfig(properties);
						config.setMaximumPoolSize(
							switch (BenchmarkEnvironment.$()) {
								case Citrine -> Runtime.getRuntime().availableProcessors() * 2;
								case Azure -> 16;
								case Unknown -> Runtime.getRuntime().availableProcessors() * 2;
							}
						);
					} catch (Exception e) {
						throw new RuntimeException("Error loading HikariCP configuration", e);
					}
					dataSource = new HikariDataSource(config);
				}
			} finally {
				dataSourceLock.unlock();
			}
		}

		return dataSource;

	}

	private static final String SelectRandomWorld = "SELECT id, randomnumber FROM world WHERE id = ?";
	private static final String UpdateRandomWorld = "UPDATE world SET randomnumber = ? WHERE id = ?";
	private static final String SelectFortunes = "SELECT id, message FROM fortune";

	private final String database;

	private HikariDataSource dataSource = null;	// lazy init
	private final Lock dataSourceLock = new ReentrantLock();

}