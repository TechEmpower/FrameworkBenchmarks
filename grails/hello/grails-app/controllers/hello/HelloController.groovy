package hello

import grails.converters.JSON
import grails.transaction.Transactional
import groovy.transform.CompileStatic
import groovy.transform.TypeCheckingMode;

import java.util.concurrent.ThreadLocalRandom

import org.springframework.transaction.annotation.Isolation;

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
        def random = ThreadLocalRandom.current()

        List<Integer> worldIds = new ArrayList<Integer>(queries)
        for (int i = 0; i < queries; i++) {
            worldIds.add(random.nextInt(10000) + 1)
        }
        List<World> worlds
        if (updateAlso) {
            worlds = getAllLocked(worldIds as Serializable[])
            for (World world : worlds) {
                world.randomNumber = random.nextInt(10000) + 1
            }
        } else {
            worlds = World.getAll(worldIds as Serializable[])
        }
        return worlds
    }
    
    @CompileStatic(TypeCheckingMode.SKIP)
    private List<World> getAllLocked(Serializable[] worldIds) {
        World.withCriteria {
            'in'('id', worldIds as Serializable[])
            lock true
        }
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
    def updates(int queries) {
        def worlds = updateWorlds(queries)
        render worlds as JSON
    }

    @Transactional(isolation=Isolation.READ_COMMITTED)
    private List updateWorlds(int queries) {
        fetchRandomWorlds(queries, true)
    }
    
    // Test type 6: Plaintext
    def plaintext() {
        render text:'Hello, world', contentType:'text/plain'
    }
}
