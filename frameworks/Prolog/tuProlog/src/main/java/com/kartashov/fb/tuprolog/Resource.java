package com.kartashov.fb.tuprolog;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.kartashov.fb.tuprolog.visitors.TermToJavaObjectConverterVisitor;
import io.vertx.core.http.HttpServerResponse;
import it.unibo.tuprolog.core.Term;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Objects;

public record Resource(String predicate, Writer writer) {
    public record Writer(String contentType) {
        public void write(HttpServerResponse response, Term term) {
            response.putHeader("Server", "tuProlog");
            response.putHeader("Date", DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
            response.putHeader("Content-Type", contentType);
            switch (contentType) {
                case "application/json" -> {
                    var visitor = new TermToJavaObjectConverterVisitor();
                    var value = term.accept(visitor);
                    var mapper = new ObjectMapper();
                    try {
                        response.end(mapper.writeValueAsString(value));
                    } catch (JsonProcessingException e) {
                        throw new RuntimeException(e);
                    }
                }
                case "text/plain" -> response.end(Objects.requireNonNull(term.asAtom()).getValue());
            }
        }
    }
}
