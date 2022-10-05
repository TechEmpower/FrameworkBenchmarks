package io.helidon.benchmark.services;

import com.fizzed.rocker.runtime.ArrayOfByteArraysOutput;
import io.helidon.benchmark.models.Fortune;
import io.helidon.benchmark.models.Repository;
import io.helidon.common.media.type.MediaTypes;
import io.helidon.nima.webserver.http.HttpRules;
import io.helidon.nima.webserver.http.HttpService;
import io.helidon.nima.webserver.http.ServerRequest;
import io.helidon.nima.webserver.http.ServerResponse;
import views.fortunes;

import java.util.Comparator;

public class FortuneService implements HttpService {

    private static final Fortune ADDITIONAL_FORTUNE = new Fortune(0, "Additional fortune added at request time.");

    private final Repository repository;

    public FortuneService(Repository repository) {
        this.repository = repository;
    }

    public void accept(ServerRequest req, ServerResponse res) {
        var fortuneList = repository.getFortunes();
        fortuneList.add(ADDITIONAL_FORTUNE);
        fortuneList.sort(Comparator.comparing(Fortune::message));

        res.headers().contentType(MediaTypes.TEXT_HTML);
        res.send(fortunes.template(fortuneList)
                .render(ArrayOfByteArraysOutput.FACTORY)
                .toByteArray());
    }

    @Override
    public void routing(HttpRules httpRules) {
        httpRules.get("/", this::accept);
    }
}