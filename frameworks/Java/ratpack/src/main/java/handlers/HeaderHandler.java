package handlers;

import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.util.AsciiString;
import ratpack.handling.Context;
import ratpack.handling.Handler;
import ratpack.http.MutableHeaders;

import java.util.Timer;
import java.util.TimerTask;

public class HeaderHandler implements Handler {
    private static final CharSequence SERVER = AsciiString.cached("server");
    private static final CharSequence RATPACK = AsciiString.cached("ratpack");

    private final DateHelper dateHelper = new DateHelper();

    @Override
    public void handle(Context ctx) {
        MutableHeaders headers = ctx.getResponse().getHeaders();
        headers.set(HttpHeaderNames.DATE, dateHelper.getDate());
        headers.set(SERVER, RATPACK);
        ctx.next();
    }

    static class DateHelper extends TimerTask {
        private Timer timer = new Timer();
        private String date = generateDate();

        public DateHelper() {
            timer.schedule(this, 0, 1000);
        }

        @Override
        public void run() {
            date = generateDate();
        }

        private String generateDate() {
            return java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.format(java.time.ZonedDateTime.now());
        }

        public String getDate() {
            return date;
        }
    }
}
