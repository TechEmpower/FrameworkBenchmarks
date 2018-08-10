package benchmark;

import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.reactivex.Single;

import java.util.HashMap;
import java.util.Map;

@Controller("/json")
public class JsonSerialization {

    @Get("/")
    Single<Map> getJson() {
        Map<String, String> map = new HashMap<>();
        map.put("message", "Hello, World!");
        return Single.just(map);
    }
}
