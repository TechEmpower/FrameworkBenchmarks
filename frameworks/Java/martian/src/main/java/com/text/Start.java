package com.text;

import com.mars.start.StartMars;
import com.text.config.TestConfig;

public class Start {

    public static void main(String[] args) {
        StartMars.start(Start.class, new TestConfig());
    }
}
