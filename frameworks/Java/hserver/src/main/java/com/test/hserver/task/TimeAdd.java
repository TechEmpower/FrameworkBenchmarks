package com.test.hserver.task;

import com.test.hserver.util.DateUtil;
import top.hserver.core.ioc.annotation.Bean;
import top.hserver.core.ioc.annotation.Task;

@Bean
public class TimeAdd {

    @Task(name = "时间计算", time = "1000")
    public void add() {
        DateUtil.time = DateUtil.getNow();
    }

}
