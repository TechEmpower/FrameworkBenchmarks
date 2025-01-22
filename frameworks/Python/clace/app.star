app = ace.app("testapp",
    routes = [
        ace.api("/json", lambda req: {'message': 'Hello, world!'}, type=ace.JSON),
        ace.api("/plaintext", lambda req: 'Hello, world!', type=ace.TEXT)
    ]
)
