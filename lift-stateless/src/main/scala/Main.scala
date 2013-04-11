package code

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
import org.streum.configrity._
import util.control.Exception._
import db._

object Main extends App {
  val opts = new Options(args)
  val defaults = new Configuration(Map[String, String](
    "db.host" -> "localhost:3306",
    "db.name" -> "hello_world",
    "db.user" -> "benchmarkdbuser",
    "db.pass" -> "benchmarkdbpass",
    "http.port" -> "8080"
  ))
  val config = defaults ++ allCatch.opt(Configuration.load("server.conf")).getOrElse(Configuration()) ++ Configuration(opts.properties)

  // initialize the db
  DB

  val server = new Server(config[Int]("http.port"))

  val src = new java.io.File("src")
  if (src.exists && src.isDirectory) {
    // dev mode
    server.setHandler(new WebAppContext("src/main/resources/", "/"))
  } else {
    // runnable jar
    server.setHandler(new WebAppContext(getClass.getClassLoader.getResource("WEB-INF/web.xml").toExternalForm.stripSuffix("WEB-INF/web.xml"), "/"))
    // setting lift's production mode
    System.setProperty("run.mode", "production")
  }


  server.start()
}
