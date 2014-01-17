package com.ibm.techempower

import java.nio.ByteBuffer

import com.ibm.plain.rest.Resource
import com.ibm.plain.text.`UTF-8`

final class PlainText

    extends Resource {

  import PlainText._

  Get { hello.duplicate }

}

object PlainText {

  private final val hello = {
    val a = "Hello, World!".getBytes(`UTF-8`)
    val buf = ByteBuffer.allocateDirect(a.length)
    buf.put(a)
    buf.flip
    buf
  }

}
