package hello.web;

import spark.Request;
import spark.Response;
import spark.ResponseTransformerRoute;

import com.google.gson.Gson;

public abstract class JsonTransformer extends ResponseTransformerRoute {

    private static final Gson   GSON              = new Gson();
    private static final String CONTENT_TYPE_JSON = "application/json";
    
    protected JsonTransformer(final String path) {
        super(path);
    }

    @Override
    public String render(final Object model) {
        return GSON.toJson(model);
    }

    @Override
    public Object handle(final Request request, final Response response) {
        response.type(CONTENT_TYPE_JSON);
        return handleInternal(request, response);
    }
    
    protected abstract Object handleInternal(Request request, Response response);
}
