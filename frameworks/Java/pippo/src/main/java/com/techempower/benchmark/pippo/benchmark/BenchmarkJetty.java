package com.techempower.benchmark.pippo.benchmark;

import ro.pippo.jetty.JettyServer;

public class BenchmarkJetty {

	public static void main(String[] args) {

		new Benchmark()
			.serverName("Jetty")
			.server(new JettyServer())
			.start();

	}

}
