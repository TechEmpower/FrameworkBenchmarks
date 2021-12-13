module DemoProcessor;

// import stdx.data.json;
import std.json;

import hunt.database;
import hunt.io;
import http.Common;
import http.Processor;
import http.HttpURI;
import http.UrlEncoded;
import hunt.logging.ConsoleLogger : trace, warning, tracef;

import std.algorithm;
import std.array;
import std.exception;
import std.random;
import std.string;

version (POSTGRESQL) {
    __gshared Database dbConnection;
}

enum HttpHeader textHeader = HttpHeader("Content-Type", "text/plain; charset=UTF-8");
enum HttpHeader htmlHeader = HttpHeader("Content-Type", "text/html; charset=UTF-8");
enum HttpHeader jsonHeader = HttpHeader("Content-Type", "application/json; charset=UTF-8");


enum plaintextLength = "/plaintext".length;
enum jsonLength = "/json".length;
enum dbLength = "/db".length;
enum fortunesLength = "/fortunes".length;

class DemoProcessor : HttpProcessor {
    version (POSTGRESQL) HttpURI uri;

    this(TcpStream client) {
        version (POSTGRESQL) uri = new HttpURI();
        super(client);
    }

    override void onComplete(ref HttpRequest req) {
        debug {
            trace(req.uri());
            trace(req.method());
            trace(req.headers());
        }

        string path = req.uri;
        // auto uri = new HttpURI(req.uri);
        // uri.parse(req.uri);
        // if(cmp(path, "/plaintext") == 0) {
        //     respondWith("Hello, World!", 200, textHeader);
        // } else if(cmp(path, "/json") == 0) {
        //     JSONValue js = JSONValue(["message" : JSONValue("Hello, World!")]);
        //     respondWith(js.toJSON(), 200, jsonHeader);
        // } else {
        //     respondWith404();
        // }
        if(path.length == plaintextLength) { // plaintext
            respondWith("Hello, World!", 200, textHeader);
        } else if(path.length == jsonLength) { // json
            JSONValue js = JSONValue(["message" : JSONValue("Hello, World!")]);
            respondWith(js.toJSON(), 200, jsonHeader);
        } else {

        version (POSTGRESQL) {
            if(path.length == dbLength) {
                respondSingleQuery();
            } else if(path.length == fortunesLength) {
                respondFortunes();
            } else {
                handleDbUpdate(path);
            }

        } else {
            respondWith404();
        }
        }
    }


    private void respondWith404() {
        version (POSTGRESQL) {
            respondWith("The available paths are: /plaintext, /json, /db, /fortunes," ~
             " /queries?queries=number, /updates?queries=number", 404);
        } else {
            respondWith("The available paths are: /plaintext, /json", 404);
        }
    }

    version (POSTGRESQL) {
        private void handleDbUpdate(string url) {
            uri.parse(url);

            switch(uri.getPath()) {
            case "/queries":
                UrlEncoded queriesMap = new UrlEncoded();
                uri.decodeQueryTo(queriesMap);
                int number = 1;
                debug {
                    trace(queriesMap.toString());
                    if (!queriesMap.containsKey("queries")) {
                        respondWith404();
                        return;
                    }

                    string v = queriesMap.getValue("queries", 0);
                    if (!v.empty) {
                        try {
                            number = to!int(v);
                        } catch (Exception ex) {
                            warning(ex.msg);
                        }
                    }
                } else {
                    string v = queriesMap.getValue("queries", 0);
                    if (!v.empty) {
                        try {
                            number = to!int(v);
                        } catch (Exception ex) {
                        }
                    }
                }

                respondMultipleQuery(number);
                break;


            case "/updates":
                UrlEncoded queriesMap = new UrlEncoded();
                uri.decodeQueryTo(queriesMap);
                int number = 1;
                debug {
                    if (!queriesMap.containsKey("queries")) {
                        respondWith404();
                        return;
                    }

                    string v = queriesMap.getValue("queries", 0);
                    if (!v.empty) {
                        try {
                            number = to!int(v);
                        } catch (Exception ex) {
                            warning(ex.msg);
                        }
                    }
                } else {
                    string v = queriesMap.getValue("queries", 0);
                    if (!v.empty) {
                        try {
                            number = to!int(v);
                        } catch (Exception ex) {
                        }
                    }
                }
                respondUpdates(number);
                break;

            default:
                respondWith404();
                break;
            }
        }


        private void respondSingleQuery() {
            int id = uniform(1, 10001);
            string query = "SELECT id, randomNumber FROM world WHERE id = " ~ id.to!string;
            ResultSet rs = dbConnection.query(query);

            JSONValue js = JSONValue(["id" : JSONValue(id), "randomNumber"
                    : JSONValue(to!int(rs.front()[0]))]);

            respondWith(js.toJSON(), 200, jsonHeader);
        }

        private void respondMultipleQuery(int queries) {
            if (queries < 1)
                queries = 1;
            else if (queries > 500)
                queries = 500;

            JSONValue[] arr = new JSONValue[queries];
            for (int i = 0; i < queries; i++) {
                immutable id = uniform(1, 10001);
                immutable query = "SELECT id, randomNumber FROM world WHERE id = " ~ id.to!string;
                ResultSet rs = dbConnection.query(query);

                arr[i] = JSONValue(["id" : JSONValue(id), "randomNumber"
                        : JSONValue(to!int(rs.front()[0]))]);
            }
            JSONValue js = JSONValue(arr);
            respondWith(js.toJSON(), 200, jsonHeader);
        }

        private void respondFortunes() {
            immutable query = "SELECT id, message::text FROM Fortune";
            ResultSet rs = dbConnection.query(query);
            FortuneModel[] data = rs.map!(f => FortuneModel(f["id"].to!int, f["message"])).array;
            data ~= FortuneModel(0, "Additional fortune added at request time.");
            data.sort!((a, b) => a.message < b.message);
            // trace(data);

            respondWith(randerFortunes(data), 200, htmlHeader);
        }

        static string randerFortunes(FortuneModel[] data) {
            Appender!string sb;
            sb.put(`<!DOCTYPE html>
<html>
	<head>
		<title>Fortunes</title>
	</head>
	<body>
		<table>
			<tr>
				<th>id</th><th>message</th>
			</tr>
`);

            foreach (FortuneModel f; data) {
                string message = replace(f.message, ">", "&gt;");
                message = replace(message, "<", "&lt;");
                message = replace(message, "\"", "&quot;");
                sb.put(format("			<tr>\n				<td>%d</td><td>%s</td>\n			</tr>\n", f.id, message));
            }

            sb.put("		</table>\n	</body>\n</html>");

            return sb.data;
        }

        private void respondUpdates(int queries) {
            if (queries < 1)
                queries = 1;
            else if (queries > 500)
                queries = 500;

            JSONValue[] arr = new JSONValue[queries];
            for (int i = 0; i < queries; i++) {
                immutable id = uniform(1, 10001);
                immutable idString = id.to!string;
                immutable query = "SELECT id, randomNumber FROM world WHERE id = " ~ idString;
                ResultSet rs = dbConnection.query(query);
                int randomNumber = to!int(rs.front()[0]);
                debug tracef("id=%d, randomNumber=%d", id, randomNumber);

                randomNumber = uniform(1, 10001);
                string updateSql = "UPDATE world SET randomNumber = "
                    ~ randomNumber.to!string ~ "  WHERE id = " ~ idString;
                int r = dbConnection.execute(updateSql);
                // debug tracef("r=%d", r);

                arr[i] = JSONValue(["id" : JSONValue(id), "randomNumber" : JSONValue(randomNumber)]);
            }

            JSONValue js = JSONValue(arr);
            respondWith(js.toJSON(), 200, jsonHeader);
        }
    }
}

struct FortuneModel {
    int id;
    string message;
}
