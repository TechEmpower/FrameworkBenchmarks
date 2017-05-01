import org.apache.commons.lang.StringEscapeUtils

def mongo = show.dbs.mongo

def generator = new Random ()

def generate = {
	Math.max(generator.nextInt(10000),1)
}

def templateHeader = """<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"""

def templateTail = """</table></body></html>"""

def validateNumQueries = {
	Integer numQueries

	if (!it) numQueries = 1

	try {
		numQueries = it[0] as Integer
	} catch (e) {
		numQueries = 1
	}
	numQueries = Math.max(1,Math.min(500,numQueries))
}

rest.get("/json").then {
	[message:'Hello, World!']
}

rest.get('/db/mongodb').then {
	mongo.world.findOne([_id:generate()]).promise()
}

rest.get('/queries/mongodb').then { req ->
	
	def numQueries = validateNumQueries(req?.requestParams?.queries)
	def deferred = show.Deferred()
	def promise = deferred.promise()
	def results = []

	(1..numQueries).each { i ->
		promise = promise.then { 
			mongo.world.findOne(_id:generate()).promise()
		}.then { world ->
			results << world
			results
		}
	}
	deferred.resolve()
	promise
}

rest.get('/fortunes/mongodb').then {

	mongo.fortune.find().promise().then { fortunes ->
		
		fortunes << [_id:'0.', message:'Additional fortune added at request time.']
		fortunes.each {
			it._id = Float.parseFloat(it._id) as Integer
			if (it.message.contains('<script>'))
				it.message = StringEscapeUtils.escapeHtml(it.message)
		}
		fortunes.sort { it.message }
		
		def response = templateHeader
		fortunes.each {
			response += "<tr><td>${it._id}</td><td>${it.message}</td></tr>"
		}
		response += templateTail
		new hot.Response(200,['Content-Type':'text/html'], response)
	}
}

rest.get('/updates/mongodb').then { req ->
	
	def numQueries = validateNumQueries(req?.requestParams?.queries)
	
	def deferred = show.Deferred()
	def promise = deferred.promise()
	def results = []

	(1..numQueries).each { i ->
		def genId = generate()

		promise = promise.then { 
			mongo.world.findOne(_id:genId).promise()
		}.then { world ->
			world.randomNumber = generate()
			mongo.world.update([_id:genId],[$set:[randomNumber:world.randomNumber]]).then {
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

def hello = 'Hello, World!'
rest.get('/plaintext').then {
	new hot.Response(200,['Content-Type':'text/plain'],hello)
}
