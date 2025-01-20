import mill._
import mill.scalalib._

def otaviaVersion = "0.4.5"

object benchmark extends ScalaModule {

    override def scalaVersion = "3.3.3"

    override def ivyDeps = Agg(
      ivy"cc.otavia::otavia-codec-http:$otaviaVersion",
      ivy"cc.otavia::otavia-postgres-driver:$otaviaVersion"
    )

}
