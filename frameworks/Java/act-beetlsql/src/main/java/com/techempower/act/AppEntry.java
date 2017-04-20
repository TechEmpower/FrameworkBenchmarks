package com.techempower.act;


import act.Act;

@SuppressWarnings("unused")
public class AppEntry {

	private static final String HELLO_WORLD = "Hello, World!";

	public static final class Message {

		private final String message;

		private Message(String message) {
			this.message = message;
		}

		public String getMessage() {
			return message;
		}

	}

	public static void main(String[] args) throws Exception {
		Act.start("ACT-BEETL Benchmark");
	}


}