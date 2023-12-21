# framework
import basolato
# middleware
import ./app/http/middlewares/session_middleware
import ./app/http/middlewares/auth_middleware
import ./app/http/middlewares/set_headers_middleware
# controller
import ./app/http/controllers/benchmark_controller


let routes = @[
  Route.get("/plaintext", benchmark_controller.plainText),
  Route.get("/json", benchmark_controller.json),
  Route.get("/db", benchmark_controller.db),
  Route.get("/queries", benchmark_controller.query),
  Route.get("/fortunes", benchmark_controller.fortune),
  Route.get("/updates", benchmark_controller.update),
]

serve(routes)
