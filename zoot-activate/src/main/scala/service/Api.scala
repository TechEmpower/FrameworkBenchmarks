package service

import scala.concurrent.Future

import net.fwbrasil.zoot.core.Api
import net.fwbrasil.zoot.core.response.Response

trait BenchmarkApi extends Api {
    
    @endpoint(path = "/db")
    def worlds(queries: String = ""): Future[String]

    @endpoint(path = "/update")
    def updateWorlds(queries: String): Future[String]

    @endpoint(path = "/fortunes")
    def fortunes: Future[String]
}
