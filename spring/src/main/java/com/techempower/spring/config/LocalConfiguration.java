package com.techempower.spring.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;

import javax.sql.DataSource;

import static org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType.H2;

@Configuration
@Profile("local")
public class LocalConfiguration {

	@Bean
	public DataSource dataSource() {
		// Using embedded H2 database for local tests
		return new EmbeddedDatabaseBuilder().setType(H2).build();
	}
}
