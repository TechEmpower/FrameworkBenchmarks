package hello;

import com.techempower.gemini.Context;
import com.techempower.gemini.path.annotation.Path;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

@Path("test")
public class GhHandler {

    @Path("plaintext")
    public String plaintext(Context context) {
        // NOTE: This is WIP Hacky Stuff - do not rely on ANY version of Gemini
        //       working this way until an official release. This is a proof of
        //       concept build to measure performance ONLY.
        context.headers().put("Server", "gemini-firenio");
        context.headers().put("Date", DateTimeFormatter.RFC_1123_DATE_TIME.format(
                ZonedDateTime.now(ZoneOffset.UTC)));

        return "Hello, World!";
    }
}
