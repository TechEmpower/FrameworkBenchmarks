import handlers.DbHandler;
import handlers.FortuneHandler;
import handlers.HeaderHandler;
import handlers.JsonHandler;
import handlers.PlainTextHandler;
import handlers.QueryHandler;
import handlers.UpdateHandler;
import ratpack.guice.Guice;
import ratpack.handlebars.HandlebarsModule;
import ratpack.hikari.HikariModule;
import ratpack.server.RatpackServer;

public class Main {
    public static void main(String... args) throws Exception {
        RatpackServer.start(server -> server
                .serverConfig(c -> c.findBaseDir("handlebars"))
                .registry(Guice.registry(b -> b
                        .module(HikariModule.class, hikariConfig -> {
                            hikariConfig.setJdbcUrl("jdbc:mysql://tfb-database:3306/hello_world?elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&cacheServerConfiguration=true&zeroDateTimeBehavior=convertToNull&traceProtocol=false&maintainTimeStats=false");
                            hikariConfig.setUsername("benchmarkdbuser");
                            hikariConfig.setPassword("benchmarkdbpass");
                            hikariConfig.setMaximumPoolSize(Runtime.getRuntime().availableProcessors()*2);
                        })
                        .module(HandlebarsModule.class)
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