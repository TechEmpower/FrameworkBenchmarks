package pronghorn.utils

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.LoggerContext
import org.slf4j.LoggerFactory

fun setLibraryLogging(level: Level) {
    val loggerContext = LoggerFactory.getILoggerFactory() as LoggerContext
    loggerContext.getLogger("org.mongodb.driver").level = level
    loggerContext.getLogger("httl").level = level
}
