package com.techempower.act;


import act.Act;
import act.Version;
import act.boot.app.RunApp;
import act.job.OnAppStart;
import com.alibaba.fastjson.JSON;
import org.osgl.http.H;

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


	@OnAppStart
	public void routing() {
		Act.get("/json", context -> context.resp()
				.contentType(H.Format.JSON.contentType())
				.writeContent(JSON.toJSONString(new Message(HELLO_WORLD))));
	}

	public static void main(String[] args) throws Exception {
		RunApp.start("Act Test", Version.appVersion(), AppEntry.class);
	}

}