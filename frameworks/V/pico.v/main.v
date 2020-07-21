import json
import syou.picoev
import syou.picohttpparser as hp

struct Message {
	message string
}

[inline]
fn json_response() string {
	msg := Message{
		message: 'Hello, World!'
	}
	return json.encode(msg)
}

[inline]
fn hello_response() string {
	return 'Hello, World!'
}

pub fn callback(req hp.Request, res mut hp.Response) {
	if hp.cmpn(req.method, 'GET ', 4) {
		if hp.cmpn(req.path, '/plaintext', 10) {
			res.http_ok().header_server().header_date().plain().body(hello_response())
		}
		else if hp.cmpn(req.path, '/json', 5) {
			res.http_ok().header_server().header_date().json().body(json_response())
		}
		else {
			res.http_404()
		}
	}
	else {
		res.http_405()
	}
}

pub fn main() {
	picoev.new(8088, &callback).serve()
}
