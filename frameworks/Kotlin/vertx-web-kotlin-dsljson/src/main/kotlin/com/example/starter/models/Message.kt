package com.example.starter.models

import com.dslplatform.json.CompiledJson
import com.dslplatform.json.JsonAttribute

@CompiledJson
class Message(@JsonAttribute(nullable = false) val message: String)
