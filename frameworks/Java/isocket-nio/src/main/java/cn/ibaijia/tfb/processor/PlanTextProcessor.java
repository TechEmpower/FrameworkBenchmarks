package cn.ibaijia.tfb.processor;

import cn.ibaijia.isocket.processor.Processor;
import cn.ibaijia.isocket.session.Session;
import cn.ibaijia.tfb.DateUtil;
import cn.ibaijia.tfb.http.HttpEntity;
import cn.ibaijia.tfb.http.HttpRequestEntity;
import cn.ibaijia.tfb.http.HttpResponseEntity;
import com.alibaba.fastjson.JSON;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PlanTextProcessor implements Processor<HttpEntity> {
    private static final Logger logger = LoggerFactory.getLogger(PlanTextProcessor.class);
    private static final String SERVER_NAME = "tfb";
    private static final String TEXT_TYPE = "text/plain";
    private static final String JSON_TYPE = "application/json";

    @Override
    public boolean process(final Session session, final HttpEntity httpEntity) {
        HttpRequestEntity httpRequestEntity = (HttpRequestEntity) httpEntity;
        String url = httpRequestEntity.url;
        logger.trace("url:{}", url);
        if (url.equals("/plaintext")) {
            HttpResponseEntity httpResponseEntity = new HttpResponseEntity();
            httpResponseEntity.setHeader("Content-Type", TEXT_TYPE);
            httpResponseEntity.setHeader("Server", SERVER_NAME);
            httpResponseEntity.setHeader("Date", DateUtil.getDate());
            httpResponseEntity.body = "Hello, World!";
            session.write(httpResponseEntity);
        } else if (url.equals("/json")) {
            HttpResponseEntity httpResponseEntity = new HttpResponseEntity();
            httpResponseEntity.setHeader("Content-Type", JSON_TYPE);
            httpResponseEntity.setHeader("Server", SERVER_NAME);
            httpResponseEntity.setHeader("Date", DateUtil.getDate());
            httpResponseEntity.body = JSON.toJSONString(new Message("Hello, World!"));
            session.write(httpResponseEntity);
        } else if (url.equals("/state")) {
            HttpResponseEntity httpResponseEntity = new HttpResponseEntity();
            httpResponseEntity.setHeader("Content-Type", JSON_TYPE);
            httpResponseEntity.setHeader("Server", SERVER_NAME);
            httpResponseEntity.setHeader("Date", DateUtil.getDate());
            State state = new State();
//            state.sessionCount = SessionManager.getSessionCount();
//            state.bufferState = new BufferState();
            httpResponseEntity.body = JSON.toJSONString(state);
            session.write(httpResponseEntity);
        } else {
            HttpResponseEntity httpResponseEntity = new HttpResponseEntity();
            httpResponseEntity.setHeader("Content-Type", TEXT_TYPE);
            httpResponseEntity.setHeader("Server", SERVER_NAME);
            httpResponseEntity.setHeader("Date", DateUtil.getDate());
            httpResponseEntity.body = "hi";
            session.write(httpResponseEntity);
        }
        return true;
    }

    @Override
    public void processError(Session session, HttpEntity httpEntity, Throwable throwable) {
        logger.error("processError:", throwable);
        HttpResponseEntity httpResponseEntity = new HttpResponseEntity();
        httpResponseEntity.setHeader("Content-Type", TEXT_TYPE);
        httpResponseEntity.setHeader("Server", SERVER_NAME);
        httpResponseEntity.setHeader("Date", DateUtil.getDate());
        httpResponseEntity.body = "hi";
//        httpResponseEntity.statusCode = 500;
//        httpResponseEntity.status = "Internal Server Error";
        session.write(httpResponseEntity);
    }
}
