
package io.helidon.benchmark.nima.services;

import java.util.Comparator;
import java.util.List;

import com.fizzed.rocker.runtime.ArrayOfByteArraysOutput;
import io.helidon.benchmark.nima.models.DbRepository;
import io.helidon.benchmark.nima.models.Fortune;
import io.helidon.nima.webserver.http.Handler;
import io.helidon.nima.webserver.http.ServerRequest;
import io.helidon.nima.webserver.http.ServerResponse;
import views.fortunes;

import static io.helidon.benchmark.nima.Main.CONTENT_TYPE_HTML;
import static io.helidon.benchmark.nima.Main.SERVER;

public class FortuneHandler implements Handler {

    private static final Fortune ADDITIONAL_FORTUNE =
            new Fortune(0, "Additional fortune added at request time.");

    private final DbRepository repository;

    public FortuneHandler(DbRepository repository) {
        this.repository = repository;
    }

    @Override
    public void handle(ServerRequest req, ServerResponse res) {
        res.header(SERVER);
        res.header(CONTENT_TYPE_HTML);
        List<Fortune> fortuneList = repository.getFortunes();
        fortuneList.add(ADDITIONAL_FORTUNE);
        fortuneList.sort(Comparator.comparing(Fortune::getMessage));
        res.send(fortunes.template(fortuneList)
                .render(ArrayOfByteArraysOutput.FACTORY)
                .toByteArray());
    }
}