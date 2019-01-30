package net.benchmark.akka.http.util

import java.util.concurrent.Executor

final class SameThreadDirectExecutor extends Executor {
  override def execute(command: Runnable): Unit = command.run()
}
