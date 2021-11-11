package net.benchmark.akka.http.util

import java.util.concurrent.Executor

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object SameThreadDirectExecutor extends Executor {

  def executionContext(): ExecutionContextExecutor = {
    ExecutionContext.fromExecutor(this)
  }

  override def execute(command: Runnable): Unit = command.run()

}
