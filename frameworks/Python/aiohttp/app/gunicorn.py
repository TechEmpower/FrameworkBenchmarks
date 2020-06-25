import uvloop

from .main import create_app

loop = uvloop.new_event_loop()
app = create_app(loop)
