package conf;

import com.google.inject.Inject;
import controllers.HelloDbController;
import controllers.HelloFortuneController;
import controllers.HelloJsonController;
import controllers.HelloPlaintextController;
import ninja.Router;
import ninja.application.ApplicationRoutes;
import ninja.utils.NinjaProperties;

public class Routes implements ApplicationRoutes {

    @Inject
    NinjaProperties ninjaProperties;

    @Override
    public void init(Router router) {

        router.GET().route("/plaintext").with(HelloPlaintextController.class, "index");
        router.GET().route("/json").with(HelloJsonController.class, "index");
        router.GET().route("/queries").with(HelloDbController.class, "multiGet");
        router.GET().route("/db").with(HelloDbController.class, "singleGet");
        router.GET().route("/fortunes").with(HelloFortuneController.class, "index");
        router.GET().route("/update").with(HelloDbController.class, "update");

    }
}
