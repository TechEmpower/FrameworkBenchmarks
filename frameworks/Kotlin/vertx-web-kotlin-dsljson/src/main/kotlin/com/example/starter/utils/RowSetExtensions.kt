package com.example.starter.utils

import io.vertx.sqlclient.Row
import io.vertx.sqlclient.RowSet

// This extension relies on the assumption the mapper never returns null, as it is defined. Otherwise,
// we prevent the overhead from having to do another iteration over the loop for a `filterNotNull` check.
@Suppress("UNCHECKED_CAST", "NOTHING_TO_INLINE")
inline fun <reified U> RowSet<Row>.mapToArray(mapper: (Row) -> U): Array<U> {
    val iterator = iterator()
    return Array(size()) { mapper(iterator.next()) }
}
