import multiprocessing
from wsgiref.handlers import format_date_time

import sanic
from sanic import response


app = sanic.Sanic()


@app.get('/json')
def json_view(request):
    return response.json({'message': 'Hello, world!'}, headers=get_headers())


@app.get('/plaintext')
def plaintext_view(request):
    return response.text('Hello, world!', headers=get_headers())


def get_headers(server='Sanic/{}'.format(sanic.__version__)):
    return {
        'Server': server,
        'Date': format_date_time(None),
    }

if __name__ == '__main__':
    app.run('0.0.0.0', 8080, access_log=False,
            workers=multiprocessing.cpu_count())
