import ujson as json


def json_endpoint(message):
    content = json.dumps({'message': 'Hello, world!'}).encode('utf-8')
    response = {
        'status': 200,
        'headers': [
            [b'content-type', b'application/json'],
        ],
        'content': content
    }
    message['reply_channel'].send(response)


def plaintext_endpoint(message):
    content = b'Hello, world!'
    response = {
        'status': 200,
        'headers': [
            [b'content-type', b'text/plain'],
        ],
        'content': content
    }
    message['reply_channel'].send(response)


def handle_404(message):
    content = b'Not found'
    response = {
        'status': 404,
        'headers': [
            [b'content-type', b'text/plain'],
        ],
        'content': content
    }
    message['reply_channel'].send(response)


routes = {
    '/json': json_endpoint,
    '/plaintext': plaintext_endpoint
}


def main(message):
    path = message['content']['path']
    routes.get(path, handle_404)(message)
