import json
import picoev
import picohttpparser

struct Message {
	message string
}

@[inline]
fn json_response(mut res picohttpparser.Response) string {
	msg := Message{
		message: 'Hello, World!'
	}
	header_ok(mut res)
	res.json()
	return json.encode(msg)
}

@[inline]
fn hello_response(mut res picohttpparser.Response) string {
	header_ok(mut res)
	res.plain()
	return 'Hello, World!'
}

@[inline]
fn index_response(mut res picohttpparser.Response) string {
	header_ok(mut res)
	res.html()
	return 'Hello!\n<a href="/plaintext">plaintext</a>\n<a href="/json">json</a>'
}

fn callback(data voidptr, req picohttpparser.Request, mut res picohttpparser.Response) {
	if req.method == 'GET' {
		if req.path == '/plaintext' {
			res.body(hello_response(mut res))
		} else if req.path == '/json' {
			res.body(json_response(mut res))
		} else {
			res.body(index_response(mut res))
		}
	} else {
		res.http_404()
	}
	res.end()
}

fn header_ok(mut res picohttpparser.Response) {
	res.http_ok()
	res.header_server()
	res.header_date()
}

fn main() {
	config := picoev.Config{
		cb: callback
	}
	println('Starting webserver on http://localhost:${config.port}/ ...')
	mut server := picoev.new(config)!
	server.serve()
}
