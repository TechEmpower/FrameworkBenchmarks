package org.smartboot.servlet;

import org.smartboot.aio.EnhanceAsynchronousChannelProvider;
import org.smartboot.http.HttpBootstrap;
import org.smartboot.http.HttpRequest;
import org.smartboot.http.HttpResponse;
import org.smartboot.http.server.Request;
import org.smartboot.http.server.handle.HttpHandle;
import org.smartboot.servlet.conf.ServletInfo;
import org.smartboot.socket.StateMachineEnum;
import org.smartboot.socket.extension.processor.AbstractMessageProcessor;
import org.smartboot.socket.transport.AioSession;

import java.io.IOException;

/**
 * @author 三刀（zhengjunweimail@163.com）
 * @version V1.0 , 2020/12/22
 */
public class Bootstrap {

    public static void main(String[] args) {
        System.setProperty("java.nio.channels.spi.AsynchronousChannelProvider", EnhanceAsynchronousChannelProvider.class.getName());

        ContainerRuntime containerRuntime = new ContainerRuntime();
        // plaintext
        ApplicationRuntime applicationRuntime = new ApplicationRuntime("/");
        ServletInfo plainTextServletInfo = new ServletInfo();
        plainTextServletInfo.setServletName("plaintext");
        plainTextServletInfo.setServletClass(HelloWorldServlet.class.getName());
        plainTextServletInfo.addMapping("/plaintext");
        applicationRuntime.getDeploymentInfo().addServlet(plainTextServletInfo);

        // json
        ServletInfo jsonServletInfo = new ServletInfo();
        jsonServletInfo.setServletName("json");
        jsonServletInfo.setServletClass(JsonServlet.class.getName());
        jsonServletInfo.addMapping("/json");
        applicationRuntime.getDeploymentInfo().addServlet(jsonServletInfo);
        containerRuntime.addRuntime(applicationRuntime);
        containerRuntime.start();
        int cpuNum = Runtime.getRuntime().availableProcessors();
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        HttpBootstrap bootstrap = new HttpBootstrap();
        bootstrap.setPort(8080).setThreadNum(cpuNum + 2)
                .setReadBufferSize(1024 * 4)
                .setReadPageSize(16384 * 1024 * 4)
                .setBannerEnabled(false)
                .setBufferPool(10 * 1024 * 1024, cpuNum + 2, 1024 * 4)
                .pipeline(new HttpHandle() {
                    @Override
                    public void doHandle(HttpRequest request, HttpResponse response) throws IOException {
                        containerRuntime.doHandle(request, response);
                    }
                })
                .wrapProcessor(processor -> new AbstractMessageProcessor<>() {
                    @Override
                    public void process0(AioSession session, Request msg) {
                        processor.process(session, msg);
                    }

                    @Override
                    public void stateEvent0(AioSession session, StateMachineEnum stateMachineEnum, Throwable throwable) {
                        processor.stateEvent(session, stateMachineEnum, throwable);
                    }
                }).start();
    }
}
