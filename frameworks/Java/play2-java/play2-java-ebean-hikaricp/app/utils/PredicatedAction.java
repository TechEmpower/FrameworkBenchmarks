package utils;

/**
 * A predicated action is one where a condition must be satisfied in order to proceed with the request. If the
 * condition is not satisfied then a supplied status result is yielded.
 */

import com.google.inject.Injector;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import javax.inject.Inject;
import play.mvc.Action;
import play.mvc.Http;
import play.mvc.Result;

public class PredicatedAction extends Action<Predicated> {

    @Inject private Injector injector;

    @Override
    public CompletionStage<Result> call(final Http.Context ctx) {
        final Predicate p = injector.getInstance(configuration.predicate());
        if (p.condition()) {
            return delegate.call(ctx);
        }
        return CompletableFuture.supplyAsync(() -> status(configuration.failed()));
    }
}
