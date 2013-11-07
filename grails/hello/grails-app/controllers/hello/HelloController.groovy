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
        def worlds = fetchRandomWorlds(queries, false)
        render worlds as JSON
    }

    private List<World> fetchRandomWorlds(int queries, boolean updateAlso) {
        if(queries < 1) queries=1
        if(queries > 500) queries=500
        List<World> worlds = new ArrayList<World>(queries)
        def random = ThreadLocalRandom.current()

        for (int i = 0; i < queries; i++) {
            int randomId = random.nextInt(10000) + 1
            def world = updateAlso ? World.get(randomId) : World.read(randomId)
            if(updateAlso) {
                world.randomNumber = random.nextInt(10000) + 1
            }
            worlds.add(world)
        }
        return worlds
    }
    
    // Test type 4: Fortunes
    @Transactional(readOnly=true)
    def fortunes() {
        def fortunes = Fortune.getAll()
        fortunes << new Fortune(message: 'Additional fortune added at request time.')
        fortunes.sort(true){Fortune it -> it.message}
        [fortunes: fortunes]
    }
    
    // Test type 5: Database updates
    @Transactional
    def updates(int queries) {
        def worlds = fetchRandomWorlds(queries, true)
        render worlds as JSON
    }
    
    // Test type 6: Plaintext
    def plaintext() {
        render text:'Hello, world', contentType:'text/plain'
    }
}
