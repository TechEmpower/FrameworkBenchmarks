package com.gitlab.zloster.tfb.servlet3.sync;

import java.io.IOException;

import javax.servlet.ServletException;
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
@WebServlet(name = "Plaintext", urlPatterns = "/plaintext")
public class Plaintext extends HttpServlet {
	private static final Logger LOGGER = LoggerFactory.getLogger(Plaintext.class);

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException,
			IOException {
		LOGGER.debug("plaintext sync start");
		resp.setContentType(Helper.MEDIATYPE_TEXT_PLAIN);
		resp.getOutputStream().write(Helper.CONTENT);
		LOGGER.debug("plaintext sync end");
	}
}