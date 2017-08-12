package hello;

import com.fizzed.rocker.ContentType;
import com.fizzed.rocker.runtime.OutputStreamOutput;
import dsl.FrameworkBench.Fortune;

import javax.servlet.ServletException;
import javax.servlet.http.*;
import java.io.IOException;
import java.util.*;

public class FortunesServlet extends HttpServlet {

	private static final Comparator<Fortune> COMPARATOR = (o1, o2) -> o1.getMessage().compareTo(o2.getMessage());

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
		final Context ctx = Utils.getContext();
		final List<Fortune> fortunes = ctx.fortunes.search();
		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		fortunes.sort(COMPARATOR);
		OutputStreamOutput oso = new OutputStreamOutput(ContentType.HTML, res.getOutputStream(), "UTF-8");
		res.setContentType("text/html;charset=UTF-8");
		views.fortunes.template(fortunes).render((t, n) -> oso);
	}
}
