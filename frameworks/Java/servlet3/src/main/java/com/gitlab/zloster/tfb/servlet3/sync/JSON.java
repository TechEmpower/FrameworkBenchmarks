package com.gitlab.zloster.tfb.servlet3.sync;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gitlab.zloster.tfb.servlet3.HelloMessage;
import com.gitlab.zloster.tfb.servlet3.Helper;

/**
 * Web Framework Benchmarks Test type 1: JSON serialization
 *
 */
@SuppressWarnings("serial")
@WebServlet("/json")
public class JSON extends HttpServlet {
	private static final Logger LOGGER = LoggerFactory.getLogger(JSON.class);
	private static final ObjectMapper mapper = new ObjectMapper();

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException,
			IOException {
		LOGGER.debug("JSON sync start");
		resp.setContentType(Helper.MEDIATYPE_APPLICATION_JSON);
		mapper.writeValue(resp.getOutputStream(), new HelloMessage());
		LOGGER.debug("JSON sync end");
	}
}