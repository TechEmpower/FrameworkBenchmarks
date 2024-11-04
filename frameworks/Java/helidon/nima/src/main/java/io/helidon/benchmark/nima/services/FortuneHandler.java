
package io.helidon.benchmark.nima.services;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

import com.fizzed.rocker.runtime.ArrayOfByteArraysOutput;
import io.helidon.benchmark.nima.models.DbRepository;
import io.helidon.benchmark.nima.models.Fortune;
import io.helidon.common.buffers.BufferData;
import io.helidon.webserver.http.Handler;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;
import views.fortunes;

import static io.helidon.benchmark.nima.Main.CONTENT_TYPE_HTML;
import static io.helidon.benchmark.nima.Main.SERVER;
import static io.helidon.http.HeaderNames.CONTENT_LENGTH;

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
        Collections.sort(fortuneList);
        ArrayOfByteArraysOutput output = fortunes.template(fortuneList).render(ArrayOfByteArraysOutput.FACTORY);
        List<byte[]> entity = output.getArrays();
        BufferData bufferData = BufferData.create(entity.stream().map(BufferData::create).toList());
        int length = bufferData.available();
        res.header(CONTENT_LENGTH, String.valueOf(length));
        try (var out = res.outputStream()) {
            bufferData.writeTo(out);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}