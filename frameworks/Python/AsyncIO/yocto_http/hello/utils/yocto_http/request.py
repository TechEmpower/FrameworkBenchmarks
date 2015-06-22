class Request:
    def __init__(self, app, path, params, headers, reader, writer, encoding='utf-8'):
        self.app = app
        self.path = path
        self.params = params
        self.headers = headers
        self.reader = reader
        self.writer = writer
        self.encoding = encoding
