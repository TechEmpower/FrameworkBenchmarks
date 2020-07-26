# framework
import basolato/routing
# middleware
import app/middlewares/custom_headers_middleware
import app/middlewares/framework_middleware
# controller
import app/controllers/benchmark_controller

settings:
  port = Port(8080)

routes:
  # Framework
  error Http404: http404Route
  error Exception: exceptionRoute
  before: framework

  get "/json": route(newBenchmarkController(request).json())
  get "/plaintext": route(newBenchmarkController(request).plainText())
  get "/db": route(newBenchmarkController(request).db())
  get "/queries": route(newBenchmarkController(request).query())
  get "/fortunes": route(newBenchmarkController(request).fortune())
  get "/updates": route(newBenchmarkController(request).update())
