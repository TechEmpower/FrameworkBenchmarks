package com.ibm.techempower

import com.ibm.plain.rest.Resource
import com.ibm.plain.json.{ Json => J }

final class Json

    extends Resource {

  Get { J(Map("message" -> "Hello, World!")) }

}
