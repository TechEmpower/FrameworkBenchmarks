package handlers;

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
    public void handle(Context ctx, DataSource datasource) {
        Blocking.get(() -> {
            try (Connection connection = datasource.getConnection()) {
                return getFortunes(connection);
            }
        }).then(fortunes -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            fortunes.sort(Comparator.comparing(fortune -> fortune.message));
            ctx.render(Template.handlebarsTemplate("fortunes", Collections.singletonMap("fortunes", fortunes), "text/html; charset=UTF-8"));
        });
    }

    private List<Fortune> getFortunes(Connection connection) {
        try {
            PreparedStatement statement = connection.prepareStatement("SELECT id, message FROM fortune");
            ResultSet rs = statement.executeQuery();

            List<Fortune> fortunes = new ArrayList<>();
            while (rs.next()) {
                fortunes.add(new Fortune(rs.getInt(1), rs.getString(2)));
            }
            statement.close();
            return fortunes;
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }
}
