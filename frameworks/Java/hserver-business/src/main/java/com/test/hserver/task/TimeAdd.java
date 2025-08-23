package com.test.hserver.task;

import cn.hserver.core.ioc.annotation.Component;
import cn.hserver.core.scheduling.annotation.Task;
import com.test.hserver.util.DateUtil;

@Component
public class TimeAdd {

    @Task(name = "时间计算", time = "1000")
    public void add() {
        DateUtil.time = DateUtil.getNow();
    }

}
