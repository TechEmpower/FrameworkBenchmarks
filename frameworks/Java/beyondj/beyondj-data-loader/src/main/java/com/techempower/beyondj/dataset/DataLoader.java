package com.techempower.beyondj.dataset;

import org.springframework.context.support.ClassPathXmlApplicationContext;

public class DataLoader {

    public static void main(String[] args) {

        new ClassPathXmlApplicationContext(
                "classpath*:**/beyondj-data-loader-jpa-spring-config.xml");
        System.out.println("Finished loading data. Bye");
    }
}
