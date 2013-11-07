package com.techempower.spring.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.sql.DataSource;

@Configuration
@Profile("bench")
public class BenchConfiguration {

	@Bean
	public DataSource dataSource() throws Exception {
		// Using JNDI lookup for pre-installed MySQL on bench env
		Context ctx = new InitialContext();
		return (DataSource) ctx.lookup("java:jdbc/hello_world");
	}
}
