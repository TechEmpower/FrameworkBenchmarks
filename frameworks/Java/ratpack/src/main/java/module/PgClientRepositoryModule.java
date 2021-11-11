package module;

import com.google.inject.AbstractModule;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import io.reactiverse.rxjava.pgclient.PgClient;
import models.JdbcRepository;
import models.PgClientRepository;

import javax.sql.DataSource;

public class PgClientRepositoryModule extends AbstractModule {
    @Provides
    @Singleton
    public PgClientRepository pgClientRepository(PgClients pgClients) {
        return new PgClientRepository(pgClients);
    }

    @Override
    protected void configure() {
    }
}
