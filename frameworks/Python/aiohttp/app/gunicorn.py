import asyncio

from .main import create_app

loop = asyncio.get_event_loop()
app = create_app(loop)
