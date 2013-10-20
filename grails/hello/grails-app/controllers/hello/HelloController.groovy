package hello

import grails.converters.JSON
import groovy.transform.CompileStatic
import java.util.concurrent.ThreadLocalRandom

@CompileStatic
class HelloController {

    def index() {
      def msg = [
        message: "Hello, world"
      ]
      render msg as JSON
    }

    def db(int queries) {
      if(queries < 1) queries=1
      def worlds = new ArrayList(queries)
      def random = ThreadLocalRandom.current();

      for (int i = 0; i < queries; i++) {
        worlds.add(World.read(random.nextInt(10000) + 1));
      }
      render worlds as JSON
    }
    
    def json() {
      def msg = [
        message: "Hello, world"
      ]
      render msg as JSON
    }
}
