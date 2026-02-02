package io.helidon.benchmark.reactive.services;

import java.nio.charset.StandardCharsets;
import java.util.Comparator;

import com.fizzed.rocker.runtime.ArrayOfByteArraysOutput;
import io.helidon.benchmark.reactive.models.DbRepository;
import io.helidon.benchmark.reactive.models.Fortune;
import io.helidon.common.http.MediaType;
import io.helidon.webserver.Handler;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;
import views.fortunes;

public class FortuneService implements Service, Handler {

    private final DbRepository repository;

    public FortuneService(DbRepository repository) {
        this.repository = repository;
    }

    @Override
    public void update(Routing.Rules rules) {
        rules.get("/fortunes", this);
    }

    @Override
    public void accept(ServerRequest req, ServerResponse res) {
        res.headers().contentType(MediaType.TEXT_HTML.withCharset(StandardCharsets.UTF_8.name()));
        repository.getFortunes()
                .forSingle(fortuneList -> {
                    fortuneList.add(new Fortune(0, "Additional fortune added at request time."));
                    fortuneList.sort(Comparator.comparing(Fortune::getMessage));
                    res.headers().contentType(MediaType.TEXT_HTML.withCharset(StandardCharsets.UTF_8.name()));
                    res.send(fortunes.template(fortuneList)
                            .render(ArrayOfByteArraysOutput.FACTORY)
                            .toByteArray());
                });
    }
}