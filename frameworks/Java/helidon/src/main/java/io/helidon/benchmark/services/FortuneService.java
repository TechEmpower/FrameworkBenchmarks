package io.helidon.benchmark.services;

import com.github.mustachejava.Mustache;
import io.helidon.benchmark.models.DbRepository;
import io.helidon.benchmark.models.Fortune;
import io.helidon.common.http.DataChunk;
import io.helidon.common.http.MediaType;
import io.helidon.common.reactive.ReactiveStreamsAdapter;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;
import io.reactivex.Single;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;

import static java.util.Comparator.comparing;

public class FortuneService implements Service {

    private final DbRepository repository;
    private final Mustache template;

    public FortuneService(DbRepository repository, Mustache template) {
        this.repository = repository;
        this.template = template;
    }

    @Override
    public void update(Routing.Rules rules) {
        rules.get("/fortunes", this::fortunes);
    }

    private void fortunes(ServerRequest request,
                          ServerResponse response) {
        Single<DataChunk> result = repository.getFortunes()
                .map(fortunes -> {
                    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
                    fortunes.sort(comparing(fortune -> fortune.message));
                    return fortunes;
                })
            .map(fortunes -> this.getChunk(fortunes));

        send(response, result);
    }

    private DataChunk getChunk(List<Fortune> fortunes) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        OutputStreamWriter writer = new OutputStreamWriter(baos, StandardCharsets.UTF_8.name());

        template.execute(writer, Collections.singletonMap("fortunes", fortunes));
        writer.flush();

        return DataChunk.create(baos.toByteArray());
    }

    private void send(final ServerResponse response, Single<DataChunk> result) {
        response.headers().contentType(MediaType.TEXT_HTML.withCharset(StandardCharsets.UTF_8.name()));
        response.send(ReactiveStreamsAdapter.publisherToFlow(result.toFlowable()));
    }
}
