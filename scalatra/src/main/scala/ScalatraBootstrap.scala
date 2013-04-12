import hello.{DbController, JsonController}
import javax.servlet.ServletContext
import org.scalatra.LifeCycle

class ScalatraBootstrap extends LifeCycle {

  override def init(context: ServletContext) {

    context.mount(new JsonController(), "/json")
    context.mount(new DbController(), "/db")

  }
}
