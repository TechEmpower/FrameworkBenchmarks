package com.text.api;

import com.mars.common.annotation.api.MarsApi;
import com.text.api.vo.MessageVO;

@MarsApi(refBean = "testService")
public interface TestApi {

    MessageVO json();
}
