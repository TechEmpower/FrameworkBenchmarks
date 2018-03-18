package object config {

  final case class Config(host: String, port: Int, apiRoot: String)

  val conf: Config = pureconfig.loadConfigOrThrow[Config]("http4s")

}
