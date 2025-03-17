
import multiprocessing
import mrhttp
import mrjson as json

app = mrhttp.Application()

@app.route('/json', _type="json")
def j(r):
  return json.dumps({'message': 'Hello, world!'})

@app.route('/plaintext', _type="text", options=['cache'])
def p(r):
  return "Hello, world!"


app.run('0.0.0.0', 8080, cores=multiprocessing.cpu_count())

