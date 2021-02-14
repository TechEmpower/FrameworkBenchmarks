package benchmark;

import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.scheduling.TaskExecutors;
import io.micronaut.scheduling.annotation.ExecuteOn;

@Controller("/plaintext")
@ExecuteOn(TaskExecutors.IO)
public class PlainText {

    private static final String TEXT = "Hello, World!";

    @Get(value = "/", produces = MediaType.TEXT_PLAIN)
    String getPlainText() {
        return TEXT;
    }
}
