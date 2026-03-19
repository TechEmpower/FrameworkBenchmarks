from web_framework_api import *


class PlainText(StatelessExecutor):
    def do_get(self, request, response):
        response.add_header("Content-Type", "text/plain")

        response.set_body("Hello, World!")
