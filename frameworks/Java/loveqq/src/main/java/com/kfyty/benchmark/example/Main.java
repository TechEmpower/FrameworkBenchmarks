package com.kfyty.benchmark.example;

import com.kfyty.loveqq.framework.boot.K;
import com.kfyty.loveqq.framework.core.autoconfig.annotation.BootApplication;
import com.kfyty.loveqq.framework.web.core.autoconfig.annotation.EnableWebMvc;

@EnableWebMvc
@BootApplication
public class Main {

    public static void main(String[] args) {
        K.start(Main.class, args);
    }
}
