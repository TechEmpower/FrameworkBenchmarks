package org.jetbrains.ktor.benchmarks

import io.jstach.jstache.JStache

@JStache(path = "fortunes.mustache")
data class Fortunes(val fortunes: List<Fortune>)
