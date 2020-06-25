import asyncio
import uvloop

from .main import create_app

uvloop.install()
loop = asyncio.get_event_loop()
app = create_app(loop)
