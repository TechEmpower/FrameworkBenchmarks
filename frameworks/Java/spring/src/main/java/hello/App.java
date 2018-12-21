package hello;

import com.zaxxer.hikari.HikariDataSource;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.context.annotation.Bean;

import javax.sql.DataSource;

@SpringBootApplication(exclude = DataSourceAutoConfiguration.class)
public class App {

    public static void main(String[] args) {
        SpringApplication.run(App.class, args);
    }

    @Bean
    public DataSource datasource(DataSourceProperties dataSourceProperties) {
        HikariDataSource dataSource = (HikariDataSource) dataSourceProperties.initializeDataSourceBuilder().type(HikariDataSource.class).build();
        dataSource.setMaximumPoolSize(Runtime.getRuntime().availableProcessors() * 2);

        return dataSource;
    }
}
