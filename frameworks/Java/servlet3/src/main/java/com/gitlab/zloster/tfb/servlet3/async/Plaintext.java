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

import com.gitlab.zloster.tfb.servlet3.Helper;

/**
 * Web Framework Benchmarks Test type 6: Plaintext
 *
 */
@SuppressWarnings("serial")
@WebServlet(name = "Plaintext", urlPatterns = "/plaintext", asyncSupported = true)
public class Plaintext extends HttpServlet {
	private static final Logger LOGGER = LoggerFactory.getLogger(Plaintext.class);

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException,
			IOException {
		final AsyncContext async = req.startAsync();
		final ServletOutputStream out = resp.getOutputStream();
		LOGGER.debug("plaintext async start");
		resp.setContentType(Helper.MEDIATYPE_TEXT_PLAIN);

		out.setWriteListener(new WriteListener() {
			@Override
			public void onWritePossible() throws IOException {
				byte[] buffer = new byte[32];
				ByteArrayInputStream is = new ByteArrayInputStream(Helper.CONTENT);

				while (out.isReady()) {
					int len = is.read(buffer);

					if (len < 0) {
						async.complete();
						LOGGER.debug("plaintext async end");
						return;
					}
					out.write(buffer, 0, len);
				}
			}

			@Override
			public void onError(Throwable t) {
				LOGGER.error("plaintext async error", t);
				async.complete();
			}
		});
	}
}