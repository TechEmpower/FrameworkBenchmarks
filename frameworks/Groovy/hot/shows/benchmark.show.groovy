import org.apache.commons.lang3.StringEscapeUtils

show.scale()

def mongo = show.dbs.mongo
def pgsqldb = show.dbs.pgsql
def mysql = show.dbs.mysql

def generator = new Random ()

def generate = {
	Math.max(generator.nextInt(10000),1)
}

def validateNumQueries = {
	Integer numQueries

	if (!it) numQueries = 1

	try {
		numQueries = it[0] as Integer
	} catch (e) {
		numQueries = 1
	}
	Math.max(1,Math.min(500,numQueries))
}

def query = { db, idLabel = '_id' ->
    db.world.findOne([(idLabel):generate()])
}

rest.get('/db/mongodb').then { query mongo }
rest.get('/db/pgsql').then { query pgsqldb, 'id' }
rest.get('/db/mysql').then { query mysql, 'id' }

def queries = { req, db, idLabel = '_id' ->

    def numQueries = validateNumQueries(req?.requestParams?.queries)
    def deferred = show.Deferred()
    def promise = deferred.promise()
    def results = []

    (1..numQueries).each { i ->
        promise = promise.then {
            db.world.findOne((idLabel):generate())
        }.then { world ->
            results << world
            results
        }
    }
    deferred.resolve()
    promise
}

rest.get('/queries/mongodb').then { req -> queries req, mongo }
rest.get('/queries/pgsql').then { req -> queries req, pgsqldb, 'id' }
rest.get('/queries/mysql').then { req -> queries req, mysql, 'id' }

def template = { content ->
    """<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>
$content
</table></body></html>
"""
}

def fortune = { db, idLabel = '_id', parseId = { it } ->
    db.fortune.find().promise().then { fortunes ->

        fortunes << [(idLabel):'0', message:'Additional fortune added at request time.']
        response = template(fortunes
                .sort { it.message }
                .collect({
                    "<tr><td>${parseId(it[idLabel])}</td><td>${StringEscapeUtils.escapeHtml4(it.message)}</td></tr>"
                }).join('')
        )
        new hot.Response(200,['Content-Type':'text/html'], response)
    }
}

rest.get('/fortunes/mongodb').then {
    fortune mongo, '_id', { Float.parseFloat(it) as Integer }
}
rest.get('/fortunes/pgsql').then { fortune pgsqldb, 'id' }
rest.get('/fortunes/mysql').then { fortune mysql, 'id' }

def update = { req, db, idLabel = '_id' ->
    def numQueries = validateNumQueries(req?.requestParams?.queries)

    def deferred = show.Deferred()
    def promise = deferred.promise()
    def results = []

    (1..numQueries).each { i ->
        def genId = generate()

        promise = promise.then {
            db.world.findOne((idLabel):genId).promise()
        }.then { world ->
            world.randomNumber = generate()
            db.world.update([(idLabel):genId],[$set:[randomNumber:world.randomNumber]]).then {
                world
            }
        }.then { world ->
            results << world
            results
        }
    }
    deferred.resolve()
    promise
}

rest.get('/updates/mongodb').then { req -> update req, mongo }
rest.get('/updates/pgsql').then { req -> update req, pgsqldb, 'id' }
rest.get('/updates/mysql').then { req -> update req, mysql, 'id' }

rest.get("/json").then {
    [message:'Hello, World!']
}

rest.get('/plaintext').then {
    new hot.Response(200,['Content-Type':'text/plain'],'Hello, World!')
}