import cgi
import json
from random import randint
from operator import attrgetter

from WebKit.Page import Page
from DbSession import Database
from AFortune import AFortune

class fortune(Page):
        def writeHTML(self):
                output = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"
                self.response().clearHeaders()
                self.response()._headers["Content-Type"] = "text/html; charset=UTF-8"
                fortunes = Database.DbSession.query(AFortune).all()
                fortunes.append(AFortune(id=0, message="Additional fortune added at request time."))
                fortunes.sort(key=attrgetter("message"))
                for fortune in fortunes:
                        message = cgi.escape(fortune.message)
                        output += "<tr><td>%s</td><td>%s</td></tr>" % (fortune.id , message.encode("utf-8"))
                output += "</table></body></html>"
                self.response()._headers["Content-Length"] = len(output)
                self.writeln(output)
