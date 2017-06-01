package org.jetbrains.ktor.benchmarks

import org.jetbrains.ktor.application.*
import org.jetbrains.ktor.http.*
import org.jetbrains.ktor.logging.*
import org.jetbrains.ktor.pipeline.*
import org.jetbrains.ktor.request.*
import org.jetbrains.ktor.util.*
import java.util.concurrent.atomic.*

// utility logging feature useful for debugging benchmark
internal class SamplingCallLogging(private val log: ApplicationLog) {
    private val counter = AtomicLong()
    var samplingFactor = 10000L

    fun log(call: ApplicationCall) {
        val v = counter.incrementAndGet()
        if (v < 0) counter.set(0)

        if (v % samplingFactor == 0L) {
            logSuccess(call)
        }
    }

    private fun logSuccess(call: ApplicationCall) {
        val status = call.response.status() ?: "Unhandled"

        when (status) {
            HttpStatusCode.Found -> log.trace("$status: ${call.request.logInfo()} -> ${call.response.headers[HttpHeaders.Location]}")
            else -> log.trace("$status: ${call.request.logInfo()}")
        }
    }

    private fun ApplicationRequest.logInfo() = "${httpMethod.value} - ${path()}"

    companion object : ApplicationFeature<Application, SamplingCallLogging, SamplingCallLogging> {
        override val key = AttributeKey<SamplingCallLogging>("SamplingCallLogging")

        override fun install(pipeline: Application, configure: SamplingCallLogging.() -> Unit): SamplingCallLogging {
            pipeline.environment.monitor.logEvents()

            val feature = SamplingCallLogging(pipeline.log)
            configure(feature)

            val loggingPhase = PipelinePhase("SLogging")

            pipeline.phases.insertBefore(ApplicationCallPipeline.Infrastructure, loggingPhase)
            pipeline.intercept(loggingPhase) { call ->
                proceed()
                feature.log(call)
            }

            return feature
        }

        private fun ApplicationMonitor.logEvents() {
            applicationStarted += { it.log.trace("Application started: $it") }
            applicationStopped += { it.log.trace("Application stopped: $it") }
            applicationStarting += { it.log.trace("Application starting: $it") }
            applicationStopping += { it.log.trace("Application stopping: $it") }
        }
    }
}