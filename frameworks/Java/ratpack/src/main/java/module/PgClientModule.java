package module;

import com.google.inject.Provides;
import com.google.inject.Singleton;
import io.reactiverse.pgclient.PgPoolOptions;
import io.reactiverse.reactivex.pgclient.PgClient;
import io.vertx.reactivex.core.Vertx;
import ratpack.guice.ConfigurableModule;

import java.util.ArrayList;
import java.util.List;

public class PgClientModule extends ConfigurableModule<PgPoolOptions> {

    @Provides
    @Singleton
    public Vertx vertx() {
        return Vertx.vertx();
    }

    @Provides
    @Singleton
    public PgClients pgClients(PgPoolOptions options, Vertx vertx) {
        List<PgClient> clients = new ArrayList<>();

        for (int i = 0; i < Runtime.getRuntime().availableProcessors(); i++) {
            clients.add(PgClient.pool(vertx, options));
        }

        return new PgClients(clients);
    }

    @Override
    protected void configure() {

    }

}
