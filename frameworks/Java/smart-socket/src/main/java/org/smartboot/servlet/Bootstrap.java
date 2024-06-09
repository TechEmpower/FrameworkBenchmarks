package org.smartboot.servlet;

import org.smartboot.http.server.HttpBootstrap;
import org.smartboot.http.server.HttpRequest;
import org.smartboot.http.server.HttpResponse;
import org.smartboot.http.server.HttpServerHandler;
import org.smartboot.servlet.conf.ServletInfo;

import java.util.concurrent.CompletableFuture;

/**
 * @author 三刀（zhengjunweimail@163.com）
 * @version V1.0 , 2020/12/22
 */
public class Bootstrap {

    public static void main(String[] args) throws Throwable {
        ContainerRuntime containerRuntime = new ContainerRuntime();
        // plaintext
        ServletContextRuntime applicationRuntime = new ServletContextRuntime(null, Thread.currentThread().getContextClassLoader(), "/");
        applicationRuntime.setVendorProvider(response -> {

        });
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

        int cpuNum = Runtime.getRuntime().availableProcessors();
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        HttpBootstrap bootstrap = new HttpBootstrap();
        bootstrap.configuration()
                .threadNum(cpuNum)
                .bannerEnabled(false)
                .headerLimiter(0)
                .readBufferSize(1024 * 4)
                .writeBufferSize(1024 * 4)
                .readMemoryPool(16384 * 1024 * 4)
                .writeMemoryPool(10 * 1024 * 1024 * cpuNum, cpuNum);
        containerRuntime.start(bootstrap.configuration());
        bootstrap.setPort(8080)
                .httpHandler(new HttpServerHandler() {
                    @Override
                    public void handle(HttpRequest request, HttpResponse response, CompletableFuture<Object> completableFuture) throws Throwable {
                        containerRuntime.doHandle(request, response, completableFuture);
                    }
                })
                .start();
    }
}
