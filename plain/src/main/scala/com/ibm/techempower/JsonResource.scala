package com.ibm.techempower

import com.ibm.plain.rest.Resource
import com.ibm.plain.json.Json

final class JsonResource

  extends Resource {

  Get { Json(Map("message" -> "Hello, World!")) }

}
