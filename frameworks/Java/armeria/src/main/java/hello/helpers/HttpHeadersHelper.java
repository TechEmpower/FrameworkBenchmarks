package hello.helpers;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import com.linecorp.armeria.common.HttpHeaderNames;
import com.linecorp.armeria.common.HttpHeaders;
import com.linecorp.armeria.common.HttpStatus;
import com.linecorp.armeria.common.MediaType;

public class HttpHeadersHelper {
  public static HttpHeaders getHttpHeader(MediaType mediaType) {
    return HttpHeaders
        .of(HttpStatus.OK)
        .add(HttpHeaderNames.SERVER, "armeria")
        .add(HttpHeaderNames.DATE,
             DateTimeFormatter.RFC_1123_DATE_TIME
                 .format(ZonedDateTime.now(ZoneOffset.UTC)))
        .contentType(mediaType);
  }
}
