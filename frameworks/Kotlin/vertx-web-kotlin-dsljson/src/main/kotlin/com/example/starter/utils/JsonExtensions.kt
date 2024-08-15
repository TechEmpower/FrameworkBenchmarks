package com.example.starter.utils

import com.dslplatform.json.DslJson
import com.dslplatform.json.runtime.Settings
import com.example.starter.io.BufferOutputStream
import io.vertx.core.buffer.Buffer

val DSL_JSON: DslJson<Any> = DslJson(
    Settings.withRuntime<Any>()
        .includeServiceLoader()
        .useStringValuesCache(DslJson.SimpleStringCache())
)

@Suppress("NOTHING_TO_INLINE")
inline fun <T> T.serialize(initialSizeHint: Int = 0): Buffer {
    val output = BufferOutputStream(initialSizeHint)
    DSL_JSON.serialize(this, output)
    return output
}
