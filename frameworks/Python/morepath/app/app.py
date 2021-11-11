from more.pony import PonyApp
from more.jinja2 import Jinja2App


class App(PonyApp, Jinja2App):
    pass


@App.template_directory()
def get_template_directory():
    return 'templates'
