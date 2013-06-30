package com.ibm.techempower

import com.ibm.plain.rest.Resource
import com.ibm.plain.text.`UTF-8`

final class PlainTextResource

  extends Resource {
  
  import PlainTextResource._

  Get { hello }

}

object PlainTextResource {
  
  final val hello = "Hello, World!".getBytes(`UTF-8`)  
  
}
