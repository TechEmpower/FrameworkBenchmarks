from aiohttp import web
from asyncworker import App

app = App()


@app.http.get(["/plaintext"])
async def handler(request: web.Request) -> web.Response:
    return web.Response(body="Hello, World!")


@app.http.get(["/json"])
async def handler(request: web.Request) -> web.Response:
    return web.json_response({"message": "Hello, World!"})


if __name__ == "__main__":
    app.run()
