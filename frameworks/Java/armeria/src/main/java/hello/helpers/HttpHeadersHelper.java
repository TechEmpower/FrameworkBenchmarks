package hello.helpers;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import com.linecorp.armeria.common.HttpHeaderNames;
import com.linecorp.armeria.common.HttpStatus;
import com.linecorp.armeria.common.MediaType;
import com.linecorp.armeria.common.ResponseHeaders;

public class HttpHeadersHelper {
  public static ResponseHeaders getHttpHeader(MediaType mediaType) {
    return ResponseHeaders.of(HttpStatus.OK,
    		HttpHeaderNames.CONTENT_TYPE, mediaType,
    		HttpHeaderNames.SERVER, "armeria",
    		HttpHeaderNames.DATE, DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now(ZoneOffset.UTC)));
  }
}
