
from WebKit.HTTPContent import HTTPContent

class plaintext(HTTPContent):
    def defaultAction(self):
    	self.response().clearHeaders()
        self.response()._headers["Content-Type"] = "text/plain; charset=UTF-8"
        self.write("Hello, World!")
