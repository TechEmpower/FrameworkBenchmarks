package com.test.hserver.task;

import cn.hserver.core.ioc.annotation.Bean;
import cn.hserver.core.ioc.annotation.Task;
import com.test.hserver.util.DateUtil;

@Bean
public class TimeAdd {

    @Task(name = "时间计算", time = "1000")
    public void add() {
        DateUtil.time = DateUtil.getNow();
    }

}
