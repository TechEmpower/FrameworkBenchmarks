package com.techempower.spring;

import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.context.annotation.ComponentScan;

@ComponentScan
@EnableAutoConfiguration
public class SampleApplication {

	public static void main(String[] args) throws Exception {
		new SpringApplicationBuilder(SampleApplication.class).run(args);
	}
}