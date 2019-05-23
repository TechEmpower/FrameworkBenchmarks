package hello;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * @author denkab
 */
final class Common {
	private Common() {
	}

	private static final int cpuCount = Runtime.getRuntime().availableProcessors();

	static ExecutorService EXECUTOR = new ThreadPoolExecutor(cpuCount * 2, cpuCount * 25, 200,
			TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>(cpuCount * 100),
			new ThreadPoolExecutor.CallerRunsPolicy());
}
