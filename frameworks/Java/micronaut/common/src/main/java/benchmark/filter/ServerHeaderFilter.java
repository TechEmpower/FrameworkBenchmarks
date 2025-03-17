package benchmark.filter;

import io.micronaut.http.MutableHttpResponse;
import io.micronaut.http.annotation.Filter;
import io.micronaut.http.annotation.ResponseFilter;
import io.micronaut.http.annotation.ServerFilter;
import io.micronaut.http.netty.NettyHttpHeaders;
import io.micronaut.scheduling.annotation.Scheduled;
import io.netty.handler.codec.http.HttpHeaderNames;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

@ServerFilter(Filter.MATCH_ALL_PATTERN)
public class ServerHeaderFilter {

    String dateHeader;

    ServerHeaderFilter() {
        setDateHeader();
    }

    @Scheduled(fixedRate = "1s")
    public void setDateHeader() {
        dateHeader = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now());
    }

    @ResponseFilter
    public void addDateHeader(MutableHttpResponse<?> mutableHttpResponse) {
        NettyHttpHeaders nettyHttpHeaders = (NettyHttpHeaders) mutableHttpResponse.getHeaders();
        nettyHttpHeaders.setUnsafe(HttpHeaderNames.DATE, dateHeader);
    }

}