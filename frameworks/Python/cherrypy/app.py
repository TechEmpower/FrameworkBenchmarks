#!/usr/bin/env python
import cherrypy

class CherryPyBenchmark(object):

    @cherrypy.expose
    @cherrypy.tools.json_out()
    def json(self):
        cherrypy.response.headers["Content-Type"] = "application/json"
        json_message = {"message": "Hello, world!"}
        return json_message  

    @cherrypy.expose    
    def plaintext(self):
        return "Hello, world!"

if __name__ == "__main__":
    cherrypy.quickstart(CherryPyBenchmark())
