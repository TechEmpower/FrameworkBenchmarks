package benchmark.controller;

import benchmark.model.Fortune;
import gg.jte.TemplateOutput;
import gg.jte.generated.precompiled.JtefortunesGenerated;
import gg.jte.html.OwaspHtmlTemplateOutput;
import io.micronaut.core.annotation.NonNull;
import io.micronaut.core.type.Argument;
import io.micronaut.core.type.MutableHeaders;
import io.micronaut.http.MediaType;
import io.micronaut.http.body.MessageBodyWriter;
import io.micronaut.http.codec.CodecException;
import io.micronaut.http.netty.NettyHttpHeaders;
import jakarta.inject.Singleton;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;

@Singleton
public class FortunesBodyWriter implements MessageBodyWriter<List<Fortune>> {

    @Override
    public void writeTo(@NonNull Argument<List<Fortune>> type,
                        @NonNull MediaType mediaType,
                        List<Fortune> values,
                        @NonNull MutableHeaders outgoingHeaders,
                        @NonNull OutputStream outputStream) throws CodecException {
        outgoingHeaders.set(NettyHttpHeaders.CONTENT_TYPE, "text/html;charset=utf-8");
        TemplateOutput output = new TemplateOutput() {

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
                    outputStream.write(value);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
        };
        JtefortunesGenerated.render(new OwaspHtmlTemplateOutput(output), null, values);
    }
}
