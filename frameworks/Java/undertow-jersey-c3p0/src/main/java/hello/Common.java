package hello;

import java.util.concurrent.*;

/**
 * @author denkab
 */
public class Common
{

  private static final int cpuCount = Runtime.getRuntime().availableProcessors();

  public static ExecutorService EXECUTOR = new ThreadPoolExecutor(
      cpuCount * 2, cpuCount * 25, 200, TimeUnit.MILLISECONDS,
      new LinkedBlockingQueue<Runnable>(cpuCount * 100),
      new ThreadPoolExecutor.CallerRunsPolicy());

}
