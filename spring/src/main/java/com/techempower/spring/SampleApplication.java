package com.techempower.spring;

import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Profile;
import org.springframework.jndi.JndiObjectFactoryBean;

import javax.sql.DataSource;

@ComponentScan
@EnableAutoConfiguration
public class SampleApplication {

    public static void main(String[] args) throws Exception {
        new SpringApplicationBuilder(SampleApplication.class).run(args);
    }

    @Profile("default")
    @Bean
    JndiObjectFactoryBean defaultDataSource() {
        JndiObjectFactoryBean factoryBean = new JndiObjectFactoryBean();
        factoryBean.setJndiName("java:jdbc/hello_world");
        factoryBean.setExpectedType(DataSource.class);

        return factoryBean;
    }

}
