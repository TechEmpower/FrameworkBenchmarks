package conf;

import com.google.inject.Singleton;

import io.undertow.util.Methods;
import mangoo.io.interfaces.MangooRoutes;
import mangoo.io.routing.Router;
import controllers.ApplicationController;

@Singleton
public class Routes implements MangooRoutes {
    
	@Override
    public void routify() {
		Router.mapRequest(Methods.GET).toUrl("/").onClassAndMethod(ApplicationController.class, "index");
        Router.mapRequest(Methods.GET).toUrl("/json").onClassAndMethod(ApplicationController.class, "json");
        Router.mapRequest(Methods.GET).toUrl("/db").onClassAndMethod(ApplicationController.class, "db");
        Router.mapRequest(Methods.GET).toUrl("/queries").onClassAndMethod(ApplicationController.class, "queries");
        Router.mapRequest(Methods.GET).toUrl("/plaintext").onClassAndMethod(ApplicationController.class, "plaintext");
        Router.mapRequest(Methods.GET).toUrl("/fortunes").onClassAndMethod(ApplicationController.class, "fortunes");
        Router.mapRequest(Methods.GET).toUrl("/updates").onClassAndMethod(ApplicationController.class, "updates");
    }
}