import handlers.DbHandler;
import handlers.FortuneHandler;
import handlers.HeaderHandler;
import handlers.JsonHandler;
import handlers.PlainTextHandler;
import handlers.QueryHandler;
import handlers.UpdateHandler;
import module.JdbcRepositoryModule;
import module.PgClientModule;
import module.PgClientRepositoryModule;
import ratpack.guice.Guice;
import ratpack.handlebars.HandlebarsModule;
import ratpack.hikari.HikariModule;
import ratpack.rx2.RxRatpack;
import ratpack.server.RatpackServer;

public class Main {
    public static void main(String... args) throws Exception {
        RxRatpack.initialize();

        RatpackServer.start(server -> server
                .serverConfig(c -> {
                    c
                            .findBaseDir("application.yml")
                            .yaml("application.yml")
                            .args(args)
                            .require("/database", DatabaseConfig.class);
                })
                .registry(Guice.registry(b -> {
                            DatabaseConfig databaseConfig = b.getServerConfig().get("/database", DatabaseConfig.class);
                            ProfileConfig profileConfig = b.getServerConfig().get("/profile", ProfileConfig.class);

                            b.module(HandlebarsModule.class);

                            if (profileConfig != null) {
                                if ("pgclient".equals(profileConfig.getName())) {
                                    b.module(PgClientModule.class, options -> {
                                        options.setDatabase(databaseConfig.getSchema());
                                        options.setHost(databaseConfig.getHost());
                                        options.setPort(databaseConfig.getPort());
                                        options.setUser(databaseConfig.getUsername());
                                        options.setPassword(databaseConfig.getPassword());
                                        options.setCachePreparedStatements(true);
                                        options.setMaxSize(1);
                                    }).module(PgClientRepositoryModule.class);
                                } else if ("jdbc".equals(profileConfig.getName())) {
                                    b.module(HikariModule.class, hikariConfig -> {
                                        hikariConfig.setJdbcUrl(String.format("jdbc:postgresql://%s:%s/%s", databaseConfig.getHost(), databaseConfig.getPort(),
                                                databaseConfig.getSchema()));
                                        hikariConfig.setUsername(databaseConfig.getUsername());
                                        hikariConfig.setPassword(databaseConfig.getPassword());
                                        hikariConfig.setMaximumPoolSize(Runtime.getRuntime().availableProcessors() * 2);
                                    }).module(JdbcRepositoryModule.class);
                                }
                            }
                        }
                ))
                .handlers(chain -> chain
                        .all(new HeaderHandler())
                        .get("plaintext", new PlainTextHandler())
                        .get("json", new JsonHandler())
                        .get("db", new DbHandler())
                        .get("queries", new QueryHandler())
                        .get("fortunes", new FortuneHandler())
                        .get("updates", new UpdateHandler())
                )
        );
    }
}