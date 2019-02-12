import muffin
import json

app = muffin.Application('muffin_test')

@app.register('/plaintext')
def hello(request):
    return 'Hello, World!'

@app.register('/json')
def hello(request):
    return json.dumps({'message': 'Hello, World!'})

if __name__ == '__main__':
    app.manage()