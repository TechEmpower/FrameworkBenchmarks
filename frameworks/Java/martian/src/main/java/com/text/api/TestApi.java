package com.text.api;

import com.mars.common.annotation.api.MarsApi;
import com.mars.server.server.request.HttpMarsResponse;
import com.text.api.vo.MessageVO;

@MarsApi(refBean = "testService")
public interface TestApi {

    MessageVO json(HttpMarsResponse response);
}
