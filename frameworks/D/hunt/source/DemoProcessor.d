module DemoProcessor;

// import stdx.data.json;
import std.json;

import hunt.database;
import hunt.io;
import http.Processor;
import http.HttpURI;
import http.UrlEncoded;
import hunt.logging.ConsoleLogger: trace, warning, tracef;

import std.exception;
import std.random;


version(POSTGRESQL) {
__gshared Database dbConnection;
}

enum HttpHeader textHeader = HttpHeader("Content-Type", "text/plain");
enum HttpHeader jsonHeader = HttpHeader("Content-Type", "application/json");


class DemoProcessor : HttpProcessor {
    this(TcpStream client) {
        super(client);
    }

    override void onComplete(HttpRequest req) {
        debug trace(req.uri);
        HttpURI uri = new HttpURI(req.uri);

version(POSTGRESQL) {        
        switch (uri.getPath) {
        case "/plaintext":
            respondWith("Hello, World!", 200, textHeader);
            break;

        case "/json":
            JSONValue js = JSONValue(["message" : JSONValue("Hello, World!")]);
            respondWith(js.toJSON(), 200, jsonHeader);
            break;

       
        case "/db":
            respondSingleQuery();
            break;
        
        case "/queries":
            UrlEncoded queriesMap = new UrlEncoded();
            uri.decodeQueryTo(queriesMap);
            int number = 5;
            debug {
                trace(queriesMap.toString());
                if(!queriesMap.containsKey("queries")) {
                    respondWith404();
                    return;
                }

                string v = queriesMap.getValue("queries", 0);
                if(!v.empty) {
                    try {
                        number = to!int(v);
                    } catch(Exception ex) {
                        warning(ex.msg);
                    }
                }
            } else {
                string v = queriesMap.getValue("queries", 0);
                if(!v.empty) {
                    number = to!int(v);
                }
            }

            respondMultipleQuery(number);
            break;
        
        case "/updates":
            UrlEncoded queriesMap = new UrlEncoded();
            uri.decodeQueryTo(queriesMap);
            int number = 5;
            debug {
                if(!queriesMap.containsKey("queries")) {
                    respondWith404();
                    return;
                }

                string v = queriesMap.getValue("queries", 0);
                if(!v.empty) {
                    try {
                        number = to!int(v);
                    } catch(Exception ex) {
                        warning(ex.msg);
                    }
                }
            } else {
                string v = queriesMap.getValue("queries", 0);
                if(!v.empty) {
                    number = to!int(v);
                }
            }
            respondUpdates(number);
            break;
        // case "/fortunes":
        //     break;
        
        default:
            respondWith404();
            break;
        }

} else {
    switch (uri.getPath) {
        case "/plaintext":
            respondWith("Hello, World!", 200, textHeader);
            break;

        case "/json":
            JSONValue js = JSONValue(["message" : JSONValue("Hello, World!")]);
            respondWith(js.toJSON(), 200, jsonHeader);
            break;
        
        default:
            respondWith404();
            break;
        }    
}                
    }

    private void respondWith404(){
        respondWith("The available paths are: /plaintext, /json, /db, /queries?queries=number", 404);
    }


version(POSTGRESQL) {
    private void respondSingleQuery() {
        int id = uniform(1, 10000);
        string query = "SELECT randomNumber FROM world WHERE id = " ~  id.to!string;
        Statement statement = dbConnection.prepare(query);
        ResultSet rs = statement.query();

        JSONValue js = JSONValue(["id" : JSONValue(id), 
            "randomNumber": JSONValue(to!int(rs.front()[0]))]);

        respondWith(js.toJSON(), 200, jsonHeader);
    }

    private void respondMultipleQuery(int queries) {
        if(queries<1)
            queries = 1;
        else if(queries>500)
            queries = 500;
        
        JSONValue[] arr;
        for(int i=0; i<queries; i++) {
			immutable id = uniform(1, 10000);
			immutable query = "SELECT randomNumber FROM world WHERE id = " ~  id.to!string;
            Statement statement = dbConnection.prepare(query);
            ResultSet rs = statement.query();
            arr ~= JSONValue(["id" : JSONValue(id), 
            "randomNumber": JSONValue(to!int(rs.front()[0]))]);
		}

        JSONValue js = JSONValue(arr);
        respondWith(js.toJSON(), 200, jsonHeader);
    }

    private void respondUpdates(int queries) {
        if(queries<1)
            queries = 1;
        else if(queries>500)
            queries = 500;
        
        JSONValue[] arr;
        for(int i=0; i<queries; i++) {
			immutable id = uniform(1, 10000);
            immutable idString = id.to!string;
			immutable query = "SELECT randomNumber FROM world WHERE id = " ~  idString;
            Statement statement = dbConnection.prepare(query);
            ResultSet rs = statement.query();
            int randomNumber = to!int(rs.front()[0]);
            debug tracef("id=%d, randomNumber=%d", id, randomNumber);

            randomNumber = uniform(1, 10000);
            string updateSql = "UPDATE world SET randomNumber = " ~ randomNumber.to!string ~ 
                "  WHERE id = " ~ idString;
            int r = dbConnection.execute(updateSql);
            // debug tracef("r=%d", r);

            arr ~= JSONValue(["id" : JSONValue(id), 
            "randomNumber": JSONValue(randomNumber)]);
		}

        JSONValue js = JSONValue(arr);
        respondWith(js.toJSON(), 200, jsonHeader);
    }
}    
}
