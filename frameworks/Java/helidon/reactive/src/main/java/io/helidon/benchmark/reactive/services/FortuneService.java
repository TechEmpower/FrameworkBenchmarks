package io.helidon.benchmark.reactive.services;

import java.util.Comparator;

import io.helidon.benchmark.reactive.models.DbRepository;
import io.helidon.benchmark.reactive.models.Fortune;
import io.helidon.common.http.HttpMediaType;
import io.helidon.common.media.type.MediaTypes;
import io.helidon.reactive.webserver.Handler;
import io.helidon.reactive.webserver.Routing;
import io.helidon.reactive.webserver.ServerRequest;
import io.helidon.reactive.webserver.ServerResponse;
import io.helidon.reactive.webserver.Service;

import com.fizzed.rocker.runtime.ArrayOfByteArraysOutput;
import views.fortunes;

public class FortuneService implements Service, Handler {

    private static final Fortune ADDITIONAL_FORTUNE = new Fortune(0, "Additional fortune added at request time.");

    private static final HttpMediaType TEXT_HTML = HttpMediaType.builder()
            .mediaType(MediaTypes.TEXT_HTML)
            .charset("UTF-8")
            .build();

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
        res.headers().contentType(TEXT_HTML);
        repository.getFortunes()
                .forSingle(fortuneList -> {
                    fortuneList.add(ADDITIONAL_FORTUNE);
                    fortuneList.sort(Comparator.comparing(Fortune::getMessage));
                    res.headers().contentType(TEXT_HTML);
                    res.send(fortunes.template(fortuneList)
                            .render(ArrayOfByteArraysOutput.FACTORY)
                            .toByteArray());
                });
    }
}