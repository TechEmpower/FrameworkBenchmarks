package com.typesafe.akka.http.benchmark

import org.fusesource.scalate.Binding

trait Templating {
  def layout(uri: String, attributes: Map[String, Any] = Map.empty, extraBindings: Traversable[Binding] = Nil): String
}
