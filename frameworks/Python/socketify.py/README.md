# socketify.py
Fast WebSocket and Http/Https server using CFFI with C API from [uNetworking/uWebSockets](https://github.com/uNetworking/uWebSockets)

This project aims at High Performance PyPy3 Web Development and WebSockets

> This project will adapt the full capi from uNetworking/uWebSockets but for now it's just this.

### Overly simple hello world app
```python
from socketify import App

app = App()
app.get("/", lambda res, req: res.end("Hello World socketify from Python!"))
app.listen(3000, lambda config: print("Listening on port http://localhost:%d now\n" % config.port))
app.run()
```

### pip install

```bash
pip install git+https://github.com/cirospaciari/socketify.py.git --global-option=build_ext
#or specify PyPy3
pypy3 -m pip install git+https://github.com/cirospaciari/socketify.py.git --global-option=build_ext
#or in editable mode
pypy3 -m pip install -e git+https://github.com/cirospaciari/socketify.py.git@main#egg=socketify
```

### Install via requirements.txt

requirements.txt file content
```text
git+https://github.com/cirospaciari/socketify.py.git@main#socketify --global-option="build_ext"
```

install command
```bash
pip install -r ./requirements.txt 
#or specify PyPy3
pypy3 -m pip install -r ./requirements.txt 
```

### SSL version sample
``` python
from socketify import App, AppOptions

app = App(AppOptions(key_file_name="./misc/key.pem", cert_file_name="./misc/cert.pem", passphrase="1234"))
app.get("/", lambda res, req: res.end("Hello World socketify from Python!"))
app.listen(3000, lambda config: print("Listening on port http://localhost:%d now\n" % config.port))
app.run()
```

### Build local from source
```bash
#clone and update submodules
git clone https://github.com/cirospaciari/socketify.py.git
cd ./socketify.py
git submodule update --init --recursive --remote
#install local pip
pypy3 -m pip install . --global-option=build_ext #--no-cache-dir is an option
#install in editable mode
pypy3 -m pip install -e .
#if you want to remove
pypy3 -m pip uninstall socketify
```
