
package io.helidon.benchmark.nima.services;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Comparator;
import java.util.List;

import gg.jte.TemplateOutput;
import io.helidon.benchmark.nima.models.DbRepository;
import io.helidon.benchmark.nima.models.Fortune;
import io.helidon.webserver.http.Handler;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

import gg.jte.html.OwaspHtmlTemplateOutput;
import gg.jte.generated.precompiled.JtefortunesGenerated;

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
        try (OutputStream os = res.outputStream()) {
            JtefortunesGenerated.render(new OwaspHtmlTemplateOutput(new HelidonTemplateOutput(os)),
                    null, fortuneList);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    static class HelidonTemplateOutput implements TemplateOutput{
        private final OutputStream os;

        HelidonTemplateOutput(OutputStream os) {
            this.os = os;
        }

        @Override
        public void writeContent(String value) {
            writeBinaryContent(value.getBytes(StandardCharsets.UTF_8));
        }

        @Override
        public void writeContent(String value, int beginIndex, int endIndex) {
            writeBinaryContent(value.substring(beginIndex, endIndex).getBytes(StandardCharsets.UTF_8));
        }

        @Override
        public void writeBinaryContent(byte[] value) {
            try {
                os.write(value);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }
}