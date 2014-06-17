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
            message: "Hello, World!"
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

        int[] worldIds = new int[queries]
        for (int i = 0; i < queries; i++) {
            worldIds[i] = (random.nextInt(10000) + 1)
        }

        List<World> worlds = new ArrayList<World>(queries)
        if (updateAlso) {
            Arrays.sort(worldIds)
            for (int id : worldIds) {
                World world = World.lock(id)
                world.randomNumber = random.nextInt(10000) + 1
                worlds.add(world)
            }
        } else {
            for (int id : worldIds) {
                worlds.add(World.read(id))
            }
        }
        return worlds
    }
    
    // Test type 4: Fortunes
    @Transactional(readOnly=true)
    def fortunes() {
        def fortunes = Fortune.getAll()
        fortunes << new Fortune(id: 0, message: 'Additional fortune added at request time.')
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
        render text:'Hello, World!', contentType:'text/plain'
    }
}
