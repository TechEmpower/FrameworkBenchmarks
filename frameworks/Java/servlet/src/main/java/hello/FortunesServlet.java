package hello;

import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.annotation.Resource;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.sql.DataSource;

/**
 * Fortunes test, returns a list of fortune cookie messages fetched from a
 * database table and then composed by server-side templates.
 */
@SuppressWarnings("serial")
public class FortunesServlet extends HttpServlet {

	// Database details.
	private static final String DB_QUERY = "SELECT * FROM Fortune";
	private static final String UTF8 = "UTF-8";
	private static final String CONTENT_TYPE_HTML_UTF8 = "text/html;charset=UTF-8";

	// Database connection pool.
	@Resource(name = "jdbc/hello_world")
	private DataSource dataSource;

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException,
			IOException {
		// Set content type to JSON
		res.setCharacterEncoding(UTF8);
		res.setContentType(CONTENT_TYPE_HTML_UTF8);

		final List<Fortune> fortunes = new ArrayList<>();

		try (Connection conn = dataSource.getConnection()) {
			Common.modifySQLConnectionSettings(conn);
			try (PreparedStatement statement = conn.prepareStatement(DB_QUERY,
					ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
					ResultSet results = statement.executeQuery()) {
				while (results.next()) {
					fortunes.add(new Fortune(results.getInt("id"), results.getString("message")));
				}
			}
		} catch (SQLException sqlex) {
			throw new ServletException(sqlex);
		}

		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		Collections.sort(fortunes);

		// Set the list of Fortunes as an attribute of the request, making it
		// available to the JSP.
		req.setAttribute("fortunes", fortunes);

		// Dispatch to the JSP.
		RequestDispatcher disp = req.getRequestDispatcher("/WEB-INF/jsp/fortunes.jsp");
		disp.forward(req, res);
	}
}
