package handlers;

import models.DbRepository;
import models.Fortune;
import ratpack.exec.Blocking;
import ratpack.handlebars.Template;
import ratpack.handling.Context;
import ratpack.handling.InjectionHandler;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class FortuneHandler extends InjectionHandler {
    public void handle(Context ctx, DbRepository repository) {
        repository.fortunes().then(fortunes -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            fortunes.sort(Comparator.comparing(fortune -> fortune.message));
            ctx.render(Template.handlebarsTemplate("fortunes", Collections.singletonMap("fortunes", fortunes), "text/html; charset=UTF-8"));
        });
    }
}
