package app;

import com.google.inject.AbstractModule;

import app.controllers.DbController;
import app.db.DbManager;
import app.models.Message;
import net.javapla.jawn.core.Jawn;
import net.javapla.jawn.core.Results;
import net.javapla.jawn.core.server.ServerConfig.PERFORMANCE_MODE;
import net.javapla.jawn.core.util.Modes;

public class BenchmarkMain extends Jawn {
    private static final String message = "Hello, World!";
    private static final byte[] bytemessage = message.getBytes();
    
    String jdbcParams = 
        "jdbcCompliantTruncation=false" +
        "&elideSetAutoCommits=true" +
        "&useLocalSessionState=true" +
        "&cachePrepStmts=true" +
        "&cacheCallableStmts=true" +
        "&alwaysSendSetIsolation=false" +
        "&prepStmtCacheSize=4096" +
        "&cacheServerConfiguration=true" +
        "&prepStmtCacheSqlLimit=2048" +
        "&zeroDateTimeBehavior=convertToNull" +
        "&traceProtocol=false" +
        "&useUnbufferedInput=false" +
        "&useReadAheadInput=false" +
        "&maintainTimeStats=false" +
        "&useServerPrepStmts=true" +
        "&cacheRSMetadata=true" +
        "&useSSL=false";

    String host = "TFB-database:5432";
    String dbUrl = "jdbc:postgresql://"+host+"/hello_world";//"jdbc:mysql://"+host+"/hello_world?";
    
    // implicit constructor
    {
        env(Modes.PROD);
        server()
            .port(8080)
            .webappPath("webapp")
            .serverPerformance(PERFORMANCE_MODE.HIGHEST)
            .contextPath("/");
        
        
        get("/queries",DbController.class, "queries");
        get("/updates",DbController.class, "updates");
        
        get("/json", (context) -> Results.json(new Message(message)));
        get("/plaintext", (context) -> Results.text(bytemessage));
        
        use(new AbstractModule() {
            @Override
            protected void configure() {
                bind(DbManager.class);
            }
        });
        
        database(Modes.PROD)
            .jdbc()
            .driver("org.postgresql.Driver")
            .url(dbUrl)// + jdbcParams)
            .user("benchmarkdbuser")
            .password("benchmarkdbpass")
            .maxPoolSize(32)
            .letFrameworkHandleConnectionPool(true);
        
        database(Modes.TEST)
            .jdbc()
            .driver("com.mysql.cj.jdbc.Driver")
            .url("jdbc:mysql://172.16.0.16/hello_world?" + jdbcParams)
            .user("benchmarkdbuser")
            .password("benchmarkdbpass")
            .letFrameworkHandleConnectionPool(true);
    
        database(Modes.DEV)
            .jdbc()
            .driver("com.mysql.cj.jdbc.Driver")
            .url("jdbc:mysql://172.16.0.16/hello_world?" + jdbcParams)
            .user("benchmarkdbuser")
            .password("benchmarkdbpass")
            .letFrameworkHandleConnectionPool(true);
    }

    public static void main(String[] args) throws Exception {
        run(BenchmarkMain::new, args);
    }
}
