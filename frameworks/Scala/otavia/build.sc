import mill._
import mill.scalalib._

def otaviaVersion = "0.4.3"

object benchmark extends ScalaModule {

    override def scalaVersion = "3.3.1"

    override def ivyDeps = Agg(
      ivy"cc.otavia::otavia-codec-http:$otaviaVersion",
      ivy"cc.otavia::otavia-postgres-driver:$otaviaVersion"
    )

}
