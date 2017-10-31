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

    private static final String DB_URL = "jdbc:mysql://TFB-database:3306/hello_world?jdbcCompliantTruncation=false&amp;elideSetAutoCommits=true&amp;useLocalSessionState=true&amp;cachePrepStmts=true&amp;cacheCallableStmts=true&amp;alwaysSendSetIsolation=false&amp;prepStmtCacheSize=4096&amp;cacheServerConfiguration=true&amp;prepStmtCacheSqlLimit=2048&amp;zeroDateTimeBehavior=convertToNull&amp;traceProtocol=false&amp;useUnbufferedInput=false&amp;useReadAheadInput=false&amp;maintainTimeStats=false&amp;useServerPrepStmts&amp;cacheRSMetadata=true&amp;useSSL=false";

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
