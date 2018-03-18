package http4s.techempower.benchmark

package object config {

  final case class Config(host: String,
                          port: Int,
                          apiRoot: String,
                          corePoolSize: Int)

  val conf: Config = pureconfig.loadConfigOrThrow[Config]("http4s")

}
