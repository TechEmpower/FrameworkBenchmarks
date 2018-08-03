package com.gitlab.zloster.tfb.servlet3.async;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import javax.servlet.AsyncContext;
import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.WriteListener;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gitlab.zloster.tfb.servlet3.HelloMessage;
import com.gitlab.zloster.tfb.servlet3.Helper;

@SuppressWarnings("serial")
@WebServlet(name = "JSON", urlPatterns = "/json", asyncSupported = true)
public class JSON extends HttpServlet {
	private static final Logger LOGGER = LoggerFactory.getLogger(JSON.class);
	private static final ObjectMapper mapper = new ObjectMapper();

	/**
	 * The JSON serialization is performed on the request processing thread and
	 * the response is sent back asynchronously.
	 */
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException,
			IOException {
		final AsyncContext async = req.startAsync();
		final ServletOutputStream out = resp.getOutputStream();
		LOGGER.debug("JSON sync start");
		resp.setContentType(Helper.MEDIATYPE_APPLICATION_JSON);
		byte[] content = mapper.writeValueAsBytes(new HelloMessage());
		out.setWriteListener(new WriteListener() {
			@Override
			public void onWritePossible() throws IOException {
				byte[] buffer = new byte[32];
				ByteArrayInputStream is = new ByteArrayInputStream(content);

				while (out.isReady()) {
					int len = is.read(buffer);

					if (len < 0) {
						async.complete();
						LOGGER.debug("JSON async end");
						return;
					}
					out.write(buffer, 0, len);
				}
			}

			@Override
			public void onError(Throwable t) {
				LOGGER.error("JSON async error", t);
				async.complete();
			}
		});
	}
}