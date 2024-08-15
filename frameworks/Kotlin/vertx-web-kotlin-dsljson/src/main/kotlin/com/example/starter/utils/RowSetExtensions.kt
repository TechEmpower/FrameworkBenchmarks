package com.example.starter.utils

import io.vertx.sqlclient.Row
import io.vertx.sqlclient.RowSet

// This extension relies on the assumption the mapper never returns null, as it is defined. Otherwise,
// we prevent the overhead from having to do another iteration over the loop for a `filterNotNull` check.
@Suppress("UNCHECKED_CAST")
inline fun <reified U> RowSet<Row>.mapToArray(mapper: (Row) -> U): Array<U> {
    val arr = arrayOfNulls<U>(this.size())
    val iterator = this.iterator()
    var index = 0
    while (iterator.hasNext()) arr[index++] = mapper(iterator.next())
    return arr as Array<U>
}
