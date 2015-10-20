package app.config;

import net.javapla.jawn.core.database.DatabaseConnections;
import net.javapla.jawn.core.spi.ApplicationDatabaseBootstrap;
import net.javapla.jawn.core.util.Modes;

public class Database implements ApplicationDatabaseBootstrap {
    
    @Override
    public void dbConnections(DatabaseConnections connections) {
        
        String jdbcParams = "jdbcCompliantTruncation=false&elideSetAutoCommits=true" +
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
                "&cacheRSMetadata=true";
        
        String dbUrl = "jdbc:mysql://127.0.0.1:3306/hello_world?";
        
        connections
            .environment(Modes.prod)
            .jdbc()
            .driver("com.mysql.jdbc.Driver")
            .url(dbUrl + jdbcParams)
            .user("benchmarkdbuser")
            .password("benchmarkdbpass")
            .maxPoolSize(256)
            .minPoolSize(256);
        
        connections
            .environment(Modes.dev)
            .jdbc()
            .driver("com.mysql.jdbc.Driver")
            .url("jdbc:mysql://192.168.100.11/hello_world?" + jdbcParams)
            .user("benchmarkdbuser")
            .password("benchmarkdbpass");
    }

}
