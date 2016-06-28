package app.config;

import net.javapla.jawn.core.api.ApplicationDatabaseBootstrap;
import net.javapla.jawn.core.database.DatabaseConnections;
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
        
        String host = "127.0.0.1:3306";
        String dbUrl = "jdbc:mysql://"+host+"/hello_world?";
        
        connections
            .environment(Modes.PROD)
            .jdbc()
            .driver("com.mysql.jdbc.Driver")
            .url(dbUrl + jdbcParams)
            .user("benchmarkdbuser")
            .password("benchmarkdbpass")
            .maxPoolSize(256)
            .minPoolSize(256);
        
        connections
            .environment(Modes.TEST)
            .jdbc()
            .driver("com.mysql.jdbc.Driver")
            .url("jdbc:mysql://172.16.0.16/hello_world?" + jdbcParams)
            .user("benchmarkdbuser")
            .password("benchmarkdbpass")
            ;
        
        connections
            .environment(Modes.DEV)
            .jdbc()
            .driver("com.mysql.jdbc.Driver")
            .url("jdbc:mysql://172.16.0.16/hello_world?" + jdbcParams)
            .user("benchmarkdbuser")
            .password("benchmarkdbpass");
    }

}
