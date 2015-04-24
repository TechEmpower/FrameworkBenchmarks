import json
import bleach
from random import randint
from operator import attrgetter

from WebKit.Page import Page
from DbSession import Database
from Fortune import Fortune

class fortune(Page):
    def writeDocType(self):
        self.writeln("<!DOCTYPE html>")

    def title(self):
        return "Fortunes"

    def htBodyArgs(self):
        return ""

    def writeContent(self):
        self.response().clearHeaders()
        self.response()._headers["Content-Type"] = "text/html; charset=UTF-8"
        fortunes = Database.DbSession.query(Fortune).all()
        fortunes.append(Fortune(id=0, message="Additional fortune added at request time."))
        fortunes.sort(key=attrgetter("message"))
		
        self.writeln("<table><tr><th>id</th><th>message</th></tr>")
        for fortune in fortunes:
            message = bleach.clean(fortune.message)
            self.writeln("<tr><td>%s</td><td>%s</td></tr>" % (fortune.id , message.encode("utf-8")))
        self.writeln("</table>")
