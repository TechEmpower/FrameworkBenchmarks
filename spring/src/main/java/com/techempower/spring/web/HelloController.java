package com.techempower.spring.web;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

@Controller
public class HelloController {

	@RequestMapping(value = "/json", produces = "application/json")
	@ResponseBody
	public Message json() {
		return new Message("Hello, world");
	}

	@RequestMapping(value = "/plaintext", produces = "text/plain")
	@ResponseBody
	public String plaintext() {
		return "Hello, World!";
	}

	public static class Message {

		private final String message;

		public Message(String message) {
			this.message = message;
		}

		public String getMessage() {
			return message;
		}

	}

}
