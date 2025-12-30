package hello.web;

import org.noear.solon.SolonApp;
import org.noear.solon.annotation.Bean;
import org.noear.solon.annotation.Configuration;

@Configuration
public class WebmvcRouter {
    @Bean
    public void initRouter(SolonApp app,
                           DbHandler dbHandler,
                           JsonHandler jsonHandler,
                           TextHandler textHandler) {
        app.handler().prev(ctx -> {
            ctx.setHandled(true);
            ctx.headerSet("Server", "Solon");

            switch (ctx.path()) {
                case "/plaintext" -> textHandler.handle(ctx);
                case "/json" -> jsonHandler.handle(ctx);
                case "/db" -> dbHandler.db(ctx);
                case "/queries" -> dbHandler.queries(ctx);
                case "/updates" -> dbHandler.updates(ctx);
                case "/fortunes" -> dbHandler.fortunes(ctx);
                default -> ctx.status(404);
            }
        });
    }
}
