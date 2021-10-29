package benchmark.controller;

import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;

import java.util.HashMap;
import java.util.Map;

@Controller("/json")
public class JsonSerializationController {

    @Get
    public Map<String, String> getJson() {
        final Map<String, String> map = new HashMap<>();
        map.put("message", "Hello, World!");
        return map;
    }
}
