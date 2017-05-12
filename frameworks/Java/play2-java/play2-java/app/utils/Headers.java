package utils;

import java.util.concurrent.CompletionStage;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import play.mvc.Action;
import play.mvc.Http;
import play.mvc.Result;

public class Headers extends Action.Simple {

    private static final DateTimeFormatter RFC_1123_DATE_TIME = DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss 'GMT'").withZoneUTC();

    @Override
    public CompletionStage<Result> call(Http.Context context) {
        context.response().setHeader("Server", "Play2");
        context.response().setHeader("Date", RFC_1123_DATE_TIME.print(new DateTime()));
        return delegate.call(context);
    }
}
