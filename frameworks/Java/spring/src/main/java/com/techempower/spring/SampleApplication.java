package com.techempower.spring;

import io.undertow.Undertow;
import io.undertow.UndertowOptions;
import org.xnio.Options;

import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.web.WebMvcAutoConfiguration;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.context.embedded.undertow.UndertowBuilderCustomizer;
import org.springframework.boot.context.embedded.undertow.UndertowEmbeddedServletContainerFactory;
import org.springframework.boot.context.web.SpringBootServletInitializer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

@ComponentScan
@EnableAutoConfiguration(exclude = {WebMvcAutoConfiguration.class})
@EnableWebMvc
public class SampleApplication extends SpringBootServletInitializer {

	@Override
	protected SpringApplicationBuilder configure(SpringApplicationBuilder application) {
		return application.sources(SampleApplication.class);
	}

	public static void main(String[] args) throws Exception {
		new SpringApplicationBuilder(SampleApplication.class).run(args);
	}

	@Bean
	public UndertowEmbeddedServletContainerFactory embeddedServletContainerFactory() {
		UndertowEmbeddedServletContainerFactory factory = new UndertowEmbeddedServletContainerFactory();
		factory.addBuilderCustomizers(new UndertowBuilderCustomizer() {

			@Override
			public void customize(Undertow.Builder builder) {
				builder.setBufferSize(1024 * 16)
						.setIoThreads(Runtime.getRuntime().availableProcessors() * 2)
						.setSocketOption(Options.BACKLOG, 10000)
						.setServerOption(UndertowOptions.ALWAYS_SET_KEEP_ALIVE, false)
						.setServerOption(UndertowOptions.ALWAYS_SET_DATE, true)
						.setWorkerThreads(200);
			}

		});
		return factory;
	}

}