package cn.ibaijia.tfb.processor;

import cn.ibaijia.isocket.processor.Processor;
import cn.ibaijia.isocket.session.Session;
import cn.ibaijia.tfb.Consts;
import cn.ibaijia.tfb.http.HttpEntity;
import cn.ibaijia.tfb.http.HttpRequestEntity;
import cn.ibaijia.tfb.http.HttpResponseEntity;
import com.alibaba.fastjson.JSON;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author longzl
 */
public class PlanTextProcessor implements Processor<HttpEntity> {
    private static final Logger logger = LoggerFactory.getLogger(PlanTextProcessor.class);

    @Override
    public boolean process(final Session session, final HttpEntity httpEntity) {
        HttpRequestEntity httpRequestEntity = (HttpRequestEntity) httpEntity;
        String url = httpRequestEntity.url;
        if (Consts.URL_TEXT_PLAIN.equals(url)) {
            HttpResponseEntity httpResponseEntity = new HttpResponseEntity();
            httpResponseEntity.setContentType(Consts.TEXT_TYPE);
            httpResponseEntity.body = "Hello, World!";
            session.write(httpResponseEntity);
        } else if (Consts.URL_JSON.equals(url)) {
            HttpResponseEntity httpResponseEntity = new HttpResponseEntity();
            httpResponseEntity.setContentType(Consts.JSON_TYPE);
            httpResponseEntity.body = JSON.toJSONString(new Message("Hello, World!"));
            session.write(httpResponseEntity);
        } else {
            HttpResponseEntity httpResponseEntity = new HttpResponseEntity();
            httpResponseEntity.setContentType(Consts.TEXT_TYPE);
            httpResponseEntity.body = "hi";
            session.write(httpResponseEntity);
        }
        return true;
    }

    @Override
    public void processError(Session session, HttpEntity httpEntity, Throwable throwable) {
        logger.error("processError:", throwable);
        HttpResponseEntity httpResponseEntity = new HttpResponseEntity();
        httpResponseEntity.setContentType(Consts.TEXT_TYPE);
        httpResponseEntity.body = "hi";
        session.write(httpResponseEntity);
    }
}
