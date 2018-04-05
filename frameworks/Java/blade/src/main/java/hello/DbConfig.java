package hello;

import com.blade.Blade;
import com.blade.event.BeanProcessor;
import com.blade.ioc.annotation.Bean;
import com.blade.jdbc.Base;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

/**
 * @author biezhi
 * @date 2017/9/22
 */
@Bean
public class DbConfig implements BeanProcessor {

    private static final String DB_URL = "jdbc:mysql://tfb-database:3306/hello_world?jdbcCompliantTruncation=false&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false&maintainTimeStats=false&useServerPrepStmts&cacheRSMetadata=true&useSSL=false";

    @Override
    public void processor(Blade blade) {

        try {
            HikariConfig config = new HikariConfig();
            config.setJdbcUrl(DB_URL);
            config.setUsername("benchmarkdbuser");
            config.setPassword("benchmarkdbpass");
            config.addDataSourceProperty("cachePrepStmts", "true");
            config.addDataSourceProperty("prepStmtCacheSize", "250");
            config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");

            HikariDataSource ds = new HikariDataSource(config);
            Base.open(ds);
        } catch (Exception e){
            System.out.println("Connection database fail");
        }

    }
}
