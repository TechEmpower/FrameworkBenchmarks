package hello;

import com.blade.Blade;
import com.blade.ioc.annotation.Bean;
import com.blade.loader.BladeLoader;
import com.blade.mvc.view.template.JetbrickTemplateEngine;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import io.github.biezhi.anima.Anima;

/**
 * Application Bootstrap
 *
 * @author biezhi
 * @date 2018/10/17
 */
@Bean
public class Bootstrap implements BladeLoader {

    @Override
    public void load(Blade blade) {
        try {
            JetbrickTemplateEngine templateEngine = new JetbrickTemplateEngine();
            blade.templateEngine(templateEngine);

            HikariConfig config = new HikariConfig();

            String url                   = blade.env("jdbc.url", "");
            String username              = blade.env("jdbc.username", "");
            String password              = blade.env("jdbc.password", "");
            String cachePrepStmts        = blade.env("datasource.cachePrepStmts", "true");
            String prepStmtCacheSize     = blade.env("datasource.prepStmtCacheSize", "250");
            String prepStmtCacheSqlLimit = blade.env("datasource.prepStmtCacheSqlLimit", "2048");

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
