package net.officefloor.benchmark;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ThreadFactory;
import java.util.function.Supplier;

import net.officefloor.server.stream.impl.ThreadLocalStreamBufferPool;
import net.officefloor.web.executive.CpuCore;
import net.openhft.affinity.Affinity;

/**
 * Encapsulating the {@link Thread} affinity.
 * 
 * @author Daniel Sagenschneider
 */
public class RawWoofThreadAffinity {

	private static final boolean IS_BENCHMARK_DEBUG = Boolean.getBoolean("officefloor.benchmark.debug");

	/**
	 * Obtains the {@link ThreadFactory} with {@link Thread} affinity.
	 * 
	 * @param bufferPoolSupplier Obtains the {@link ThreadLocalStreamBufferPool}.
	 * @return {@link ThreadFactory} with {@link Thread} affinity.
	 */
	public static ThreadFactory[] createThreadFactories(Supplier<ThreadLocalStreamBufferPool> bufferPoolSupplier) {
		List<ThreadFactory> threadFactories = new LinkedList<>();
		if (IS_BENCHMARK_DEBUG) {
			// Simple threading for debug
			System.out.println("\n\nWARNING: using debug mode so performance will suffer\n\n");
			for (int i = 0; i < Runtime.getRuntime().availableProcessors(); i++) {
				threadFactories.add((runnable) -> new Thread(() -> {
					ThreadLocalStreamBufferPool bufferPool = bufferPoolSupplier.get();
					try {
						bufferPool.activeThreadLocalPooling();
						runnable.run();
					} finally {
						bufferPool.threadComplete();
					}
				}));
			}

		} else {
			// Provide socket per thread with thread affinity
			for (CpuCore cpuCore : CpuCore.getCores()) {
				for (CpuCore.LogicalCpu logicalCpu : cpuCore.getCpus()) {

					// Create thread factory for logical CPU
					ThreadFactory boundThreadFactory = (runnable) -> new Thread(() -> {
						ThreadLocalStreamBufferPool bufferPool = bufferPoolSupplier.get();
						try {
							// Bind thread to logical CPU
							Affinity.setAffinity(logicalCpu.getCpuAffinity());

							// Set up for thread local buffer pooling
							bufferPool.activeThreadLocalPooling();

							// Run logic for thread
							runnable.run();
						} finally {
							bufferPool.threadComplete();
						}
					});

					// Add the thread factory
					threadFactories.add(boundThreadFactory);
				}
			}
		}

		return threadFactories.toArray(new ThreadFactory[0]);
	}

}