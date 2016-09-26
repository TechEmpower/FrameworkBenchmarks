package highlevel;

import common.Fortune;
import org.rapidoid.http.Req;
import org.rapidoid.http.ReqRespHandler;
import org.rapidoid.http.Resp;
import org.rapidoid.render.Template;
import org.rapidoid.render.Templates;
import org.rapidoid.sql.JdbcClient;

import java.util.Collections;
import java.util.List;

public class FortunesHandler implements ReqRespHandler {

	private final Template fortunesTmpl = Templates.load("fortunes.html");

	private final JdbcClient jdbc;

	public FortunesHandler(JdbcClient jdbc) {
		this.jdbc = jdbc;
	}

	@Override
	public Object execute(Req req, Resp resp) throws Exception {

		List<Fortune> fortunes = jdbc.query(Fortune.class, "SELECT * FROM fortune");

		fortunes.add(new Fortune(0, "Additional fortune added at request time."));

		Collections.sort(fortunes);

		return fortunesTmpl.renderToBytes(fortunes);
	}

}
