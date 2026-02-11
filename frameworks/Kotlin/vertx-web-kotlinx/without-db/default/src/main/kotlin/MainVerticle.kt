import io.vertx.core.http.HttpHeaders
import io.vertx.ext.web.Router

class MainVerticle : CommonVerticle() {
    override fun Router.routes() {
        get("/json").jsonResponseCoHandler(Serializers.message) {
            Message("Hello, World!")
        }

        get("/plaintext").coHandlerUnconfined {
            it.response().run {
                headers().run {
                    addCommonHeaders()
                    add(HttpHeaders.CONTENT_TYPE, HttpHeaderValues.textPlain)
                }
                end("Hello, World!")/*.coAwait()*/
            }
        }
    }
}