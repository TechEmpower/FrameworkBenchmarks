import os
os.environ["TURBO_DISABLE_RATE_LIMITING"] = "1"

from turboapi import TurboAPI
from turboapi.responses import PlainTextResponse

app = TurboAPI()

@app.get("/json")
def json_test():
    return {"message": "Hello, World!"}

@app.get("/plaintext")
def plaintext_test():
    return PlainTextResponse(content="Hello, World!")

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8080)
