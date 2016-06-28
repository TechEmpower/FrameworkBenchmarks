
from WebKit.HTTPContent import HTTPContent

class plaintext(HTTPContent):
    def defaultAction(self):
    	self.response().clearHeaders()
        self.response()._headers["Content-Type"] = "text/plain"
        output = "Hello, World!"
        self.response()._headers["Content-Length"] = len(output)
        self.write(output)
