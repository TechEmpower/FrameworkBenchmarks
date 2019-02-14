package com.te;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import ro.pippo.core.Application;
import java.util.HashMap;
import java.util.Map;
/**
 * A simple Pippo application.
 *
 * @see com.te.PippoLauncher#main(String[])
 */
public class PippoApplication extends Application {

    private final static Logger log = LoggerFactory.getLogger(PippoApplication.class);

    @Override
    protected void onInit() {
        getRouter().ignorePaths("/favicon.ico");

        // send 'Hello World' as response
        GET("/plaintext", routeContext -> {
            routeContext.setHeader("Server", "Hippo");
            routeContext.setHeader("content-type", "text/plain");
            routeContext.send("Hello, World!");
        });

        GET("/json", routeContext -> {
            Map<String, String> greeting = new HashMap();
            greeting.put("message", "Hello, World!");
            routeContext.setHeader("Server", "Hippo");
            routeContext.json().send(greeting);
        });

        // send a template as response
        GET("/template", routeContext -> {
            String message;

            String lang = routeContext.getParameter("lang").toString();
            if (lang == null) {
                message = getMessages().get("pippo.greeting", routeContext);
            } else {
                message = getMessages().get("pippo.greeting", lang);
            }

            routeContext.setLocal("greeting", message);
            routeContext.render("hello");
        });
    }

}
