package hello

import grails.converters.JSON
import grails.transaction.Transactional
import groovy.transform.CompileStatic
import java.util.concurrent.ThreadLocalRandom

@CompileStatic
class HelloController {

    def index() {
        plaintext()
    }

    // benchmark specification
    // http://www.techempower.com/benchmarks/#section=code
    
    // Test type 1: JSON serialization
    def json() {
        def msg = [
            message: "Hello, world"
        ]
        render msg as JSON
    }

    // Test type 2: Single database query
    @Transactional(readOnly=true)
    def db() {
        def random = ThreadLocalRandom.current()
        def world = World.read(random.nextInt(10000) + 1)
        render world as JSON
    }
    
    // Test type 3: Multiple database queries
    @Transactional(readOnly=true)
    def queries(int queries) {
        if(queries < 1) queries=1
        if(queries > 500) queries=500
        def worlds = new ArrayList(queries)
        def random = ThreadLocalRandom.current()

        for (int i = 0; i < queries; i++) {
            worlds.add(World.read(random.nextInt(10000) + 1))
        }
        render worlds as JSON
    }
    
    // Test type 6: Plaintext
    def plaintext() {
        render text:'Hello, world', contentType:'text/plain'
    }
}
