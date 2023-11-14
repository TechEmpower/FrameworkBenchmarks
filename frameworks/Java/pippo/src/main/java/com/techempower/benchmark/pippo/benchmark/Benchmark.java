package com.techempower.benchmark.pippo.benchmark;

import com.techempower.benchmark.pippo.BenchmarkApplication;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import ro.pippo.core.AbstractWebServer;
import ro.pippo.core.Pippo;

public class Benchmark {

	public Benchmark() {
		BenchmarkApplication app = new BenchmarkApplication();
		pippo = new Pippo(app);
	}

	public Benchmark serverName(String serverName) {
		this.serverName = serverName;
		return this;
	}

	public Benchmark server(AbstractWebServer<?> server) {
		pippo.setServer(server);
		return this;
	}

	public Benchmark start() {
		Log.info("Starting benchmark {}...", serverName);
		pippo.start();
		Log.info("Benchmark {} started", serverName);
		return this;
	}

	public Benchmark stop() {
		pippo.stop();
		return this;
	}

	private static final Logger Log = LoggerFactory.getLogger(Benchmark.class);

	private final Pippo pippo;

	private String serverName;

}
