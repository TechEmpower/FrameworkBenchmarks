package hello;

import com.hellokaton.anima.Anima;
import com.hellokaton.blade.Blade;
import com.hellokaton.blade.ioc.annotation.Bean;
import com.hellokaton.blade.loader.BladeLoader;
import com.hellokaton.blade.template.JetbrickTemplateEngine;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

/**
 * Application Bootstrap
 *
 * @author hellokaton
 * @date 2022/5/10
 */
@Bean
public class Bootstrap implements BladeLoader {

    @Override
    public void load(Blade blade) {
        try {
            JetbrickTemplateEngine templateEngine = new JetbrickTemplateEngine();
            blade.templateEngine(templateEngine);

            HikariConfig config = new HikariConfig();

            String url                   = blade.getEnv("jdbc.url", "");
            String username              = blade.getEnv("jdbc.username", "");
            String password              = blade.getEnv("jdbc.password", "");
            String cachePrepStmts        = blade.getEnv("datasource.cachePrepStmts", "true");
            String prepStmtCacheSize     = blade.getEnv("datasource.prepStmtCacheSize", "250");
            String prepStmtCacheSqlLimit = blade.getEnv("datasource.prepStmtCacheSqlLimit", "2048");

            config.setJdbcUrl(url);
            config.setUsername(username);
            config.setPassword(password);
            config.addDataSourceProperty("cachePrepStmts", cachePrepStmts);
            config.addDataSourceProperty("prepStmtCacheSize", prepStmtCacheSize);
            config.addDataSourceProperty("prepStmtCacheSqlLimit", prepStmtCacheSqlLimit);

            HikariDataSource ds = new HikariDataSource(config);
            Anima.open(ds);
        } catch (Exception e) {
            System.out.println("Connection database fail");
        }
    }
}
