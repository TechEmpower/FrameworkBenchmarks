def json_no_args():
    return {'message': 'Hello, world!'}

def text_no_args():
    return 'Hello, world!'

app = ace.app("testapp",
    routes = [
        ace.api("/json", type=ace.JSON, handler=json_no_args),
        ace.api("/plaintext", type=ace.TEXT, handler=text_no_args)
    ]
)
