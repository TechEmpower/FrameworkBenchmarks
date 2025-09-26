package com.techempower.inverno.benchmark;

import io.inverno.core.annotation.NestedBean;
import io.inverno.mod.boot.BootConfiguration;
import io.inverno.mod.configuration.Configuration;
import io.inverno.mod.http.server.HttpServerConfiguration;

@Configuration
public interface AppConfiguration {

	@NestedBean
	BootConfiguration boot();
	
	@NestedBean
	HttpServerConfiguration http_server();
	
	default String db_database() {
		return "postgres";
	}
	
	default String db_host() {
		return "localhost";
	}
	
	default int db_port() {
		return 5432;
	}
	
	default String db_username() {
		return "postgres";
	}
	
	default String db_password() {
		return "password";
	}
}
