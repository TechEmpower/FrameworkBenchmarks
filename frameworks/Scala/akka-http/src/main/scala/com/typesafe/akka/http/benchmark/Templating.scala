package com.typesafe.akka.http.benchmark

import org.fusesource.scalate.TemplateEngine

trait Templating {
  def templateEngine: TemplateEngine
}
