package module;

import com.google.inject.AbstractModule;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import io.reactiverse.pgclient.PgPoolOptions;
import io.reactiverse.rxjava.pgclient.PgClient;
import models.JdbcRepository;
import ratpack.guice.ConfigurableModule;

import javax.sql.DataSource;

public class JdbcRepositoryModule extends AbstractModule {
    @Provides
    @Singleton
    public JdbcRepository jdbcRepository(DataSource dataSource) {
        return new JdbcRepository(dataSource);
    }

    @Override
    protected void configure() {
    }
}
