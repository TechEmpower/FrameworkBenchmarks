
from WebKit.HTTPContent import HTTPContent
import json

class json2(HTTPContent):
    def defaultAction(self):
        self.response().clearHeaders()
        self.response()._headers["Content-Type"] = "application/json"
        output = json.dumps({"message": "Hello, World!"})		
        self.response()._headers["Content-Length"] = len(output)		
        self.write(output)
