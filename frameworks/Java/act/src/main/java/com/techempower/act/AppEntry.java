package com.techempower.act;


import act.Version;
import act.boot.app.RunApp;

public class AppEntry {

	public static void main(String[] args) throws Exception {
		RunApp.start("Act Test", Version.appVersion(), AppEntry.class);
	}

}