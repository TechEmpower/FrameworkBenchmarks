package micronaut;

import io.micronaut.http.HttpRequest;
import io.micronaut.http.MutableHttpResponse;
import io.micronaut.http.annotation.Filter;
import io.micronaut.http.filter.OncePerRequestHttpServerFilter;
import io.micronaut.http.filter.ServerFilterChain;
import io.netty.util.AsciiString;
import io.reactivex.Single;
import org.reactivestreams.Publisher;

import static io.netty.handler.codec.http.HttpHeaderNames.DATE;
import static io.netty.handler.codec.http.HttpHeaderNames.SERVER;

@Filter("/**")
public class HeaderFilter extends OncePerRequestHttpServerFilter {

    private static final CharSequence SERVER_NAME = AsciiString.cached("Micronaut");

    private DateHandler dateHandler;

    public HeaderFilter(DateHandler dateHandler) {
        this.dateHandler = dateHandler;
    }

    @Override
    protected Publisher<MutableHttpResponse<?>> doFilterOnce(HttpRequest<?> request, ServerFilterChain chain) {
        return Single.fromPublisher(chain.proceed(request)).doOnSuccess(
                response -> response.header(SERVER, SERVER_NAME).header(DATE, dateHandler.getDate()))
                .toFlowable();
    }
}
