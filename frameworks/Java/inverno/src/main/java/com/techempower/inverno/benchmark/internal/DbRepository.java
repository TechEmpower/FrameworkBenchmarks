package com.techempower.inverno.benchmark.internal;

import com.techempower.inverno.benchmark.AppConfiguration;
import com.techempower.inverno.benchmark.model.Fortune;
import com.techempower.inverno.benchmark.model.World;
import io.inverno.core.annotation.Bean;
import io.inverno.core.annotation.Destroy;
import io.inverno.core.annotation.Init;
import io.inverno.mod.base.concurrent.Reactor;
import io.inverno.mod.base.concurrent.VertxReactor;
import io.inverno.mod.sql.PreparedStatement;
import io.inverno.mod.sql.SqlClient;
import io.inverno.mod.sql.vertx.ConnectionSqlClient;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgConnection;
import java.util.ArrayList;
import java.util.List;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public class DbRepository {

	public static final String DB_SELECT_WORLD = "SELECT id, randomnumber from WORLD where id = $1";
	public static final String DB_SELECT_FORTUNE = "SELECT id, message from FORTUNE";

	private final SqlClient sqlClient;

	private final PreparedStatement selectWorldByIdQuery;
	private final PreparedStatement selectFortuneQuery;
	private final PreparedStatement[] updateWorldQueries;

	public DbRepository(SqlClient sqlClient) {
		this.sqlClient = sqlClient;

		this.selectWorldByIdQuery = sqlClient.preparedStatement(DB_SELECT_WORLD);
		this.selectFortuneQuery = sqlClient.preparedStatement(DB_SELECT_FORTUNE);
		this.updateWorldQueries = new PreparedStatement[500];
		for(int i=0;i<this.updateWorldQueries.length;i++) {
			updateWorldQueries[i] = sqlClient.preparedStatement(buildAggregatedUpdateQuery(i + 1));
		}
	}

	private static String buildAggregatedUpdateQuery(int len) {
		StringBuilder sql = new StringBuilder();
		sql.append("UPDATE WORLD SET RANDOMNUMBER = CASE ID");
		for (int i = 0; i < len; i++) {
			int offset = (i * 2) + 1;
			sql.append(" WHEN $").append(offset).append(" THEN $").append(offset + 1);
		}
		sql.append(" ELSE RANDOMNUMBER");
		sql.append(" END WHERE ID IN ($1");
		for (int i = 1; i < len; i++) {
			int offset = (i * 2) + 1;
			sql.append(",$").append(offset);
		}
		sql.append(")");
		return sql.toString();
	}

	public SqlClient getSqlClient() {
		return sqlClient;
	}

	public Mono<World> getWorld(int id) {
		return Mono.from(this.selectWorldByIdQuery.bind(id).execute(row -> new World(row.getInteger(0), row.getInteger(1))));
	}

	public Flux<Fortune> listFortunes() {
		return Flux.from(this.selectFortuneQuery.execute(row -> new Fortune(row.getInteger(0), row.getString(1))));
	}

	public Mono<Void> updateWorlds(List<World> worlds) {
		int len = worlds.size();
		List<Object> parameters = new ArrayList<>(len * 2);
		for(World world : worlds) {
			parameters.add(world.getId());
			parameters.add(world.getRandomNumber());
		}
		return Mono.when(this.updateWorldQueries[len - 1].bind(parameters).execute());
	}

	@Bean( name = "dbRespository", visibility = Bean.Visibility.PRIVATE )
	public static class ReactorScope extends io.inverno.mod.base.concurrent.ReactorScope<Mono<DbRepository>> {

		private final AppConfiguration configuration;
		private final Reactor reactor;

		private Vertx vertx;
		private PgConnectOptions connectOptions;

		public ReactorScope(AppConfiguration configuration, Reactor reactor) {
			this.configuration = configuration;
			this.reactor = reactor;
		}

		@Init
		public void init() {
			if(this.reactor instanceof VertxReactor) {
				this.vertx = ((VertxReactor)this.reactor).getVertx();
			}
			else {
				this.vertx = Vertx.vertx(new VertxOptions().setPreferNativeTransport(this.configuration.boot().prefer_native_transport()));
			}

			this.connectOptions = new PgConnectOptions()
				.setHost(this.configuration.db_host())
				.setPort(this.configuration.db_port())
				.setDatabase(this.configuration.db_database())
				.setUser(this.configuration.db_username())
				.setPassword(this.configuration.db_password())
				.setCachePreparedStatements(true)
				.setPreparedStatementCacheMaxSize(1024)
				.setPipeliningLimit(100_100);
		}

		@Destroy
		public void destroy() {
			if(!(this.reactor instanceof VertxReactor)) {
				this.vertx.close();
			}
		}

		@Override
		protected Mono<DbRepository> create() {
			return Mono.fromCompletionStage(() -> PgConnection.connect(this.vertx, this.connectOptions).toCompletionStage())
				.map(pgConn -> new DbRepository(new ConnectionSqlClient(pgConn)))
				.cacheInvalidateWhen(repository -> ((ConnectionSqlClient)repository.getSqlClient()).onClose());
		}
	}
}
