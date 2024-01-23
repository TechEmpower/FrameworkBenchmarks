package io.vertx.benchmark

import io.vertx.core.http.HttpServerRequest
import java.util.*
import kotlin.math.max
import kotlin.math.min

private val RANDOM = SplittableRandom()

/**
 * Returns the value of the "queries" getRequest parameter, which is an integer
 * bound between 1 and 500 with a default value of 1.
 *
 * @param request the current HTTP request
 * @return the value of the "queries" parameter
 */
fun getQueries(request: HttpServerRequest): Int {
    val param = request.getParam("queries") ?: return 1
    return try {
        val parsedValue = param.toInt()
        min(500, max(1, parsedValue))
    } catch (e: NumberFormatException) {
        1
    }
}

/**
 * Returns a random integer that is a suitable value for both the `id`
 * and `randomNumber` properties of a world object.
 *
 * @return a random world number
 */
fun randomWorld(): Int =
    1 + RANDOM.nextInt(10000)
