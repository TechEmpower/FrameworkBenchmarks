package com.techempower.inverno.benchmark;

import java.io.IOException;
import java.util.function.Supplier;

import io.inverno.core.annotation.Bean;
import io.inverno.core.v1.Application;
import io.inverno.mod.configuration.ConfigurationSource;
import io.inverno.mod.configuration.source.BootstrapConfigurationSource;

public class Main {

	@Bean
	public interface AppConfigurationSource extends Supplier<ConfigurationSource<?, ?, ?>> {}
	
	public static void main(String[] args) throws IllegalStateException, IOException {
		Application.with(new Benchmark.Builder()
			.setAppConfigurationSource(new BootstrapConfigurationSource(Main.class.getModule(), args))
		).run();
	}
}
