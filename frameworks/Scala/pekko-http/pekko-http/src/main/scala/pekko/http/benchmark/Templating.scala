package pekko.http.benchmark

import org.fusesource.scalate.TemplateEngine

trait Templating {
  def templateEngine: TemplateEngine
}
