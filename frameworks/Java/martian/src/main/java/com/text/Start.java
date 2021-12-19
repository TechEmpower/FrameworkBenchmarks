package com.text;

import com.martian.annotation.MartianScan;
import com.martian.starter.StartMartian;
import com.text.config.TestConfig;

@MartianScan(scanPackage = "com.text.api")
public class Start {

    public static void main(String[] args) {
        StartMartian.start(Start.class, new TestConfig());
    }
}
