package hello;

import javax.sql.DataSource;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Profile;
import org.springframework.context.event.EventListener;

import com.zaxxer.hikari.HikariDataSource;

@SpringBootApplication
public class App {

	public static void main(String[] args) {
		SpringApplication.run(App.class, args);
	}

	@EventListener(ApplicationReadyEvent.class)
    public void runAfterStartup() {
        System.out.println("Application is ready");
    }

	@Bean
	@Profile({ "jdbc", "jpa" })
	DataSource datasource(DataSourceProperties dataSourceProperties) {
		HikariDataSource dataSource = dataSourceProperties.initializeDataSourceBuilder().type(HikariDataSource.class)
				.build();
		dataSource.setMaximumPoolSize(Runtime.getRuntime().availableProcessors() * 2);

		return dataSource;
	}

}
