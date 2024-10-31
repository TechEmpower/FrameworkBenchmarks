package hello.controller;

import org.noear.solon.annotation.Component;
import org.noear.solon.core.handle.Context;
import org.noear.solon.core.handle.Filter;
import org.noear.solon.core.handle.FilterChain;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

@Component
public class FilterImpl implements Filter {
    private static DateFormat DATE_FORMAT = new SimpleDateFormat("EEE, d MMM yyyyy HH:mm:ss z");

    @Override
    public void doFilter(Context ctx, FilterChain chain) throws Throwable {
        String dateString = DATE_FORMAT.format(new Date());
        ctx.headerSet("Date", dateString);
        ctx.headerSet("Server", "solon-boot-vertx");
        chain.doFilter(ctx);
    }
}
