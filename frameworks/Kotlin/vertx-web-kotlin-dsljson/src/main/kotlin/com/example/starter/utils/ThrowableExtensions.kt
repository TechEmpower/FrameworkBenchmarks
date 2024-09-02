package com.example.starter.utils

import io.netty.channel.unix.Errors
import io.netty.channel.unix.Errors.NativeIoException
import java.net.SocketException

const val CONNECTION_RESET_MESSAGE = "Connection reset"

@Suppress("NOTHING_TO_INLINE")
inline fun Throwable.isConnectionReset(): Boolean {
    return when {
        this is NativeIoException && this.expectedErr() == Errors.ERRNO_ECONNRESET_NEGATIVE -> true
        this is SocketException && this.message == CONNECTION_RESET_MESSAGE -> true
        else -> false
    }
}
