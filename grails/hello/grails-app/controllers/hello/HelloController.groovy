package hello

import grails.converters.JSON
import java.util.concurrent.ThreadLocalRandom

class HelloController {

    def index() {
      def response = [
        message: "Hello, world"
      ]
      render response as JSON
    }

    def db() {
      int queries = params.queries ? params.int('queries') : 1
      def worlds = new ArrayList(queries)
      def random = ThreadLocalRandom.current();

      for (int i = 0; i < queries; i++) {
        worlds.add(World.read(random.nextInt(10000) + 1));
      }
      render worlds as JSON
    }
    
    def json() {
      def response = [
        message: "Hello, world"
      ]
      render response as JSON
    }
}
