package com.techempower.benchmark.pippo;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class BenchmarkEnvironment {

	// see: https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Environment

	public enum Environment {
		Citrine,
		Azure,
		Unknown
	}

	public static Environment $() {

		if (env == null) {
			envLock.lock();
			try {
				if (env == null)
					env = calculate();
			} finally {
				envLock.unlock();
			}
		}

		return env;

	}

	private static Environment calculate() {

		String envRaw = System.getenv("BENCHMARK_ENV");

		if (StringUtils.isBlank(envRaw)) {
			Log.info("No benchmark environment set");
			return Environment.Unknown;
		}

		try {
			return Environment.valueOf(envRaw);
		} catch (IllegalArgumentException e) {
			Log.warn("Unknown benchmark environment: '{}'", envRaw);
			return Environment.Unknown;
		}

	}

	private static final Logger Log = LoggerFactory.getLogger(BenchmarkEnvironment.class);

	private static final Lock envLock = new ReentrantLock();
	private static Environment env = null;

}
