package com.techempower.benchmark.pippo.benchmark;

import ro.pippo.tomcat.TomcatServer;

public class BenchmarkTomcat {

	public static void main(String[] args) {
		new Benchmark()
			.serverName("Tomcat")
			.server(new TomcatServer())
			.start();
	}

}