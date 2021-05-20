package cn.ibaijia.tfb.processor;

import cn.ibaijia.isocket.processor.Processor;
import cn.ibaijia.isocket.session.Session;
import cn.ibaijia.tfb.http.HttpEntity;
import cn.ibaijia.tfb.http.HttpRequestEntity;
import cn.ibaijia.tfb.http.HttpResponseEntity;
import cn.ibaijia.tfb.protocol.SimpleHttpProtocol;
import com.alibaba.fastjson.JSON;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

public class PlanTextProcessor implements Processor<HttpEntity> {
    private static final Logger logger = LoggerFactory.getLogger(PlanTextProcessor.class);
    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss z", Locale.ENGLISH);
    private static final String SERVER_NAME = "isocket-nio-tfb";
    private static Message message = new Message("Hello, World!");

    @Override
    public boolean process(Session session, HttpEntity httpEntity) {
        HttpRequestEntity httpRequestEntity = (HttpRequestEntity) httpEntity;
        logger.trace("url:{}", httpRequestEntity.url);
        if (httpRequestEntity.url.contains("/plaintext")) {
            HttpResponseEntity httpResponseEntity = new HttpResponseEntity();
            httpResponseEntity.setHeader("Content-Type", "text/plain; charset=UTF-8");
            httpResponseEntity.setHeader("Server", SERVER_NAME);
            httpResponseEntity.setHeader("Date", dateFormat.format(new Date()));
            httpResponseEntity.body = "Hello, World!";
            session.write(httpResponseEntity);
        } else if (httpRequestEntity.url.contains("/json")) {
            HttpResponseEntity httpResponseEntity = new HttpResponseEntity();
            httpResponseEntity.setHeader("Content-Type", "application/json; charset=UTF-8");
            httpResponseEntity.setHeader("Server", SERVER_NAME);
            httpResponseEntity.setHeader("Date", dateFormat.format(new Date()));
            httpResponseEntity.body = JSON.toJSONString(message);
            session.write(httpResponseEntity);
        } else {
            HttpResponseEntity httpResponseEntity = new HttpResponseEntity();
            httpResponseEntity.setHeader("Content-Type", "text/plain");
            httpResponseEntity.setHeader("Server", SERVER_NAME);
            httpResponseEntity.setHeader("Date", dateFormat.format(new Date()));
            httpResponseEntity.body = "hi";
            session.write(httpResponseEntity);
        }
//        String connection = httpRequestEntity.getHeader("Connection");
//        logger.trace("Connection:{}", connection);
//        if (connection == null || "close".equals(connection)) {
//            session.close(false); //TODO
//        }
        return true;
    }

    @Override
    public void processError(Session session, HttpEntity httpEntity, Throwable throwable) {
        logger.error("processError:", throwable);
        HttpResponseEntity httpResponseEntity = new HttpResponseEntity();
        httpResponseEntity.setHeader("Content-Type", "text/plain");
        httpResponseEntity.setHeader("Server", SERVER_NAME);
        httpResponseEntity.setHeader("Date", dateFormat.format(new Date()));
        httpResponseEntity.body = "hi";
        httpResponseEntity.statusCode = 500;
        httpResponseEntity.status = "Internal Server Error";
        session.write(httpResponseEntity);
    }
}
