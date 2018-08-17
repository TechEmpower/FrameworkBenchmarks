package benchmark;

import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.reactivex.Single;

@Controller("/plaintext")
public class PlainText {

    private static final String TEXT = "Hello, World!";

    @Get(value = "/", produces = MediaType.TEXT_PLAIN)
    Single<String> getPlainText() {
        return Single.just(TEXT);
    }
}
