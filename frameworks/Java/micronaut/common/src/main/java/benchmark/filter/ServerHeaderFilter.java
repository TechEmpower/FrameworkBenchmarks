package benchmark.filter;

import io.micronaut.core.async.publisher.Publishers;
import io.micronaut.http.HttpRequest;
import io.micronaut.http.MutableHttpResponse;
import io.micronaut.http.annotation.Filter;
import io.micronaut.http.filter.HttpServerFilter;
import io.micronaut.http.filter.ServerFilterChain;
import io.micronaut.scheduling.annotation.Scheduled;
import org.reactivestreams.Publisher;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.function.Function;

@Filter(Filter.MATCH_ALL_PATTERN)
public class ServerHeaderFilter implements HttpServerFilter {

    private volatile Function<MutableHttpResponse<?>, MutableHttpResponse<?>> addHeader;

    ServerHeaderFilter() {
        setDateHeader();
    }

    @Scheduled(fixedRate = "1s")
    public void setDateHeader() {
        addHeader = new Function<>() {

            private final String dateHeader = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now());

            @Override
            public MutableHttpResponse<?> apply(MutableHttpResponse<?> mutableHttpResponse) {
                return mutableHttpResponse.header("Date", dateHeader);
            }
        };
    }

    @Override
    public Publisher<MutableHttpResponse<?>> doFilter(HttpRequest<?> request, ServerFilterChain chain) {
        return Publishers.map(chain.proceed(request), addHeader);
    }

}