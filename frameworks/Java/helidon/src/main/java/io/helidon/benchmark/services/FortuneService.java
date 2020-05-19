package io.helidon.benchmark.services;

import static java.util.Comparator.comparing;

import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.Collections;

import com.github.mustachejava.Mustache;

import io.helidon.benchmark.models.DbRepository;
import io.helidon.benchmark.models.Fortune;
import io.helidon.common.http.MediaType;
import io.helidon.webserver.Routing;
import io.helidon.webserver.ServerRequest;
import io.helidon.webserver.ServerResponse;
import io.helidon.webserver.Service;

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

    private void fortunes(ServerRequest request, ServerResponse response) {
        repository.getFortunes()
                .subscribe(fortunes -> {
                    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
                    fortunes.sort(comparing(fortune -> fortune.message));

                    ByteArrayOutputStream baos = new ByteArrayOutputStream();
                    OutputStreamWriter writer = new OutputStreamWriter(baos, StandardCharsets.UTF_8.name());

                    template.execute(writer, Collections.singletonMap("fortunes", fortunes));
                    writer.flush();

                    response.headers().contentType(MediaType.TEXT_HTML.withCharset(StandardCharsets.UTF_8.name()));
                    response.send(baos.toByteArray());
                });
    }
}
