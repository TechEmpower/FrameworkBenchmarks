package hellowicket;

import org.apache.wicket.core.request.handler.PageProvider;
import org.apache.wicket.core.request.handler.RenderPageRequestHandler;
import org.apache.wicket.request.IRequestHandler;
import org.apache.wicket.request.IRequestMapper;
import org.apache.wicket.request.Request;
import org.apache.wicket.request.Url;
import org.apache.wicket.request.handler.resource.ResourceReferenceRequestHandler;

import hellowicket.dbupdates.HelloDbUpdatesReference;
import hellowicket.fortune.FortunePage;
import hellowicket.plaintext.HelloTextReference;

/**
 * Custom request mapper optimized for the application needs
 */
public class RequestMapper implements IRequestMapper {

    private static final Url FORTUNES_URL = Url.parse("fortunes");

    @Override
    public IRequestHandler mapRequest(final Request request) {
        final String url = request.getUrl().getPath();
        switch (url) {
            case "json":
                return new ResourceReferenceRequestHandler(new HelloJsonReference());
            case "db":
                return new ResourceReferenceRequestHandler(new HelloDbReference());
            case "updates":
                return new ResourceReferenceRequestHandler(new HelloDbUpdatesReference());
            case "plaintext":
                return new ResourceReferenceRequestHandler(new HelloTextReference());
            case "fortunes":
                return new RenderPageRequestHandler(new PageProvider(FortunePage.class));
            default:
        }
        return null;
    }

    @Override
    public int getCompatibilityScore(final Request request) {
        return 0;
    }

    @Override
    public Url mapHandler(final IRequestHandler requestHandler) {
        return FORTUNES_URL;
    }
}
