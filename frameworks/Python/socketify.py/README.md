# socketify.py


<p align="center">
  <a href="https://github.com/cirospaciari/socketify.py"><img src="https://raw.githubusercontent.com/cirospaciari/socketify.py/main/misc/logo.png" alt="Logo" height=170></a>
  <br />
  <br />
  <a href="https://github.com/cirospaciari/socketify.py/actions/workflows/linux.yml" target="_blank"><img src="https://github.com/cirospaciari/socketify.py/actions/workflows/linux.yml/badge.svg" /></a>
  <a href="https://github.com/cirospaciari/socketify.py/actions/workflows/windows.yml" target="_blank"><img src="https://github.com/cirospaciari/socketify.py/actions/workflows/windows.yml/badge.svg" /></a>
<a href="https://github.com/cirospaciari/socketify.py/actions/workflows/macos.yml" target="_blank"><img src="https://github.com/cirospaciari/socketify.py/actions/workflows/macos.yml/badge.svg" /></a>
  <a href="https://github.com/cirospaciari/socketify.py/actions/workflows/macos_arm64.yml" target="_blank"><img src="https://github.com/cirospaciari/socketify.py/actions/workflows/macos_arm64.yml/badge.svg" /></a>
  <br/>
<a href='https://github.com/cirospaciari/socketify.py'><img alt='GitHub Clones' src='https://img.shields.io/badge/dynamic/json?color=success&label=Clones&query=count&url=https://gist.githubusercontent.com/cirospaciari/2243d59951f4abe4fd2000f1e20bc561/raw/clone.json&logo=github'></a>
<a href="https://github.com/sponsors/cirospaciari/" target="_blank"><img src="https://img.shields.io/static/v1?label=Sponsor&message=%E2%9D%A4&logo=GitHub&link=https://github.com/sponsors/cirospaciari"/></a>


</p>



## 💡 Features

- WebSocket with pub/sub support
- Fast and realiable Http/Https
- Support for Windows, Linux and macOS Silicon & x64
- Support for [`PyPy3`](https://www.pypy.org/) and [`CPython`](https://github.com/python/cpython)
- Dynamic URL Routing with Wildcard & Parameter support
- Sync and Async Function Support
- Really Simple API
- Fast and Encrypted TLS 1.3 quicker than most alternative servers can do even unencrypted, cleartext messaging
- Per-SNI HttpRouter Support
- Proxy Protocol v2
- Shared or Dedicated Compression Support
- Max Backpressure, Max Timeout, Max Payload and Idle Timeout Support
- Automatic Ping / Pong Support
- Per Socket Data
- Middlewares
- Templates Support (examples with [`Mako`](https://github.com/cirospaciari/socketify.py/tree/main/examples/template_mako.py) and [`Jinja2`](https://github.com/cirospaciari/socketify.py/tree/main/examples/template_jinja2.py))
## :mag_right: Upcoming Features
- Fetch like API powered by libuv
- Async file IO powered by libuv
- Full asyncio integration with libuv
- Full Http3 support
- [`HPy`](https://hpyproject.org/) integration to better support [`CPython`](https://github.com/python/cpython), [`PyPy`](https://www.pypy.org/) and [`GraalPython`](https://github.com/oracle/graalpython)
- Hot Reloading

We created and adapted the full C API from [uNetworking/uWebSockets](https://github.com/uNetworking/uWebSockets) and will integrate libuv powered fetch and file IO, this same C API is used by [Bun](https://bun.sh/)

Join Github [`Discussions`](https://github.com/cirospaciari/socketify.py/discussions) or [`Discord`](https://discord.socketify.dev/) for help and have a look at the development progress.

## :zap: Benchmarks
HTTP requests per second (Linux x64)

![image](https://raw.githubusercontent.com/cirospaciari/socketify.py/main/misc/bench-bar-graph.svg)

WebSocket messages per second (Linux x64)

![image](https://raw.githubusercontent.com/cirospaciari/socketify.py/main/misc/ws-bar-graph.svg)


Socketify got almost 900k messages/s with PyPy3 and 860k with Python3 the same performance as [Bun](https://bun.sh) that also uses uWebSockets, Falcon at 35k messages/s and Falcon with PyPy3 improves into 56k messages/s, node.js manages 192k.

Runtime versions: PyPy3 7.3.9, Python 3.10.7, node v16.17.0, bun v0.2.2<br/>
Framework versions: gunicorn 20.1.0 + uvicorn 0.19.0, socketify alpha, gunicorn 20.1.0 + falcon 3.1.0, robyn 0.18.3<br/>
Http tested with oha -c 40 -z 5s http://localhost:8000/ (1 run for warmup and 3 runs average for testing)<br/>
WebSocket tested with [Bun.sh](https://bun.sh) bench chat-client <br/>
Source code in [bench](https://github.com/cirospaciari/socketify.py/tree/main/bench)<br/>

Machine OS: Debian GNU/Linux bookworm/sid x86_64 Kernel: 6.0.0-2-amd64 CPU: Intel i7-7700HQ (8) @ 3.800GHz Memory: 32066MiB 

> Today socketify has get 30% performance hit due to workarounds between asyncio + libuv, so we will got even faster! See more info in [this issue](https://github.com/cirospaciari/socketify.py/issues/18), Python3 and PyPy3 performance will improve when we migrate to [HPy](https://github.com/cirospaciari/socketify.py/issues/16). In TechEmPower paintext benchmarks we are the fastest in active web framework for Python/PyPy with more than 2 millions of req/s, followed by [Robyn](https://github.com/sansyrox/robyn) with almost 1.2 million req/s and [Falcon](https://github.com/falconry/falcon/tree/master/falcon) using meinheld with about 560k req/s.
## 📦 Installation
For macOS x64 & Silicon, Linux x64, Windows

```bash
pip install git+https://github.com/cirospaciari/socketify.py.git
#or specify PyPy3
pypy3 -m pip install git+https://github.com/cirospaciari/socketify.py.git
#or in editable mode
pypy3 -m pip install -e git+https://github.com/cirospaciari/socketify.py.git@main#egg=socketify
```

Using install via requirements.txt
```text
git+https://github.com/cirospaciari/socketify.py.git@main#socketify
```
```bash
pip install -r ./requirements.txt 
#or specify PyPy3
pypy3 -m pip install -r ./requirements.txt 
```

If you are using linux or macOS, you may need to install libuv and zlib in your system

macOS
```bash
brew install libuv
brew install zlib
```

Linux
```bash
apt install libuv1 zlib1g
```

## 🤔 Usage

Hello world app
```python
from socketify import App

app = App()
app.get("/", lambda res, req: res.end("Hello World socketify from Python!"))
app.listen(3000, lambda config: print("Listening on port http://localhost:%d now\n" % config.port))
app.run()
```

SSL version sample
``` python
from socketify import App, AppOptions

app = App(AppOptions(key_file_name="./misc/key.pem", cert_file_name="./misc/cert.pem", passphrase="1234"))
app.get("/", lambda res, req: res.end("Hello World socketify from Python!"))
app.listen(3000, lambda config: print("Listening on port http://localhost:%d now\n" % config.port))
app.run()
```

WebSockets
```python
from socketify import App, AppOptions, OpCode, CompressOptions

def ws_open(ws):
    print('A WebSocket got connected!')
    ws.send("Hello World!", OpCode.TEXT)

def ws_message(ws, message, opcode):
    #Ok is false if backpressure was built up, wait for drain
    ok = ws.send(message, opcode)
    
app = App()    
app.ws("/*", {
    'compression': CompressOptions.SHARED_COMPRESSOR,
    'max_payload_length': 16 * 1024 * 1024,
    'idle_timeout': 12,
    'open': ws_open,
    'message': ws_message,
    'drain': lambda ws: print('WebSocket backpressure: %i' % ws.get_buffered_amount()),
    'close': lambda ws, code, message: print('WebSocket closed')
})
app.any("/", lambda res,req: res.end("Nothing to see here!'"))
app.listen(3000, lambda config: print("Listening on port http://localhost:%d now\n" % (config.port)))
app.run()
```

We have more than 20 examples [click here](https://github.com/cirospaciari/socketify.py/tree/main/examples) for more

## :hammer: Building from source
```bash
#clone and update submodules
git clone https://github.com/cirospaciari/socketify.py.git
cd ./socketify.py
git submodule update --init --recursive --remote
#you can use make linux, make macos or call Make.bat from Visual Studio Development Prompt to build
cd ./src/socketify/native/ && make linux && cd ../../../
#install local pip
pypy3 -m pip install .
#install in editable mode
pypy3 -m pip install -e .
#if you want to remove
pypy3 -m pip uninstall socketify
```

## :briefcase: Commercially supported
I'm a Brazilian consulting & contracting company dealing with anything related with [socketify.py](https://github.com/cirospaciari/socketify.py) and [socketify.rb](https://github.com/cirospaciari/socketify.rb)

Don't hesitate sending a mail if you are in need of advice, support, or having other business inquiries in mind. We'll figure out what's best for both parties.

Special thank's to [uNetworking AB](https://github.com/uNetworking) to develop [uWebSockets](https://github.com/uNetworking/uWebSockets), [uSockets](https://github.com/uNetworking/uSockets) and allow us to bring this features and performance to Python and PyPy

## :heart: Sponsors
If you like to see this project thrive, you can sponsor us on GitHub too. We need all the help we can get 

<a href="https://github.com/sponsors/cirospaciari/" target="_blank"><img src="https://img.shields.io/static/v1?label=Sponsor&message=%E2%9D%A4&logo=GitHub&link=https://github.com/sponsors/cirospaciari"/></a>

## :star: Stargazers
[![Stargazers repo roster for @cirospaciari/socketify.py](https://reporoster.com/stars/dark/cirospaciari/socketify.py)](https://github.com/cirospaciari/socketify.py/stargazers)

## :wrench: Forkers
[![Forkers repo roster for @cirospaciari/socketify.py](https://reporoster.com/forks/dark/cirospaciari/socketify.py)](https://github.com/cirospaciari/socketify.py/network/members)


## :question: socketify.py vs japronto

People really want to compare with japronto, but this projects are not really comparable. Socketify is an active project and will be maintained over time with security updates and new features, japronto don't get any github updates since 2020 and don't get any src update since 2018, japronto don't support SSL, WebSockets, [`PyPy3`](https://www.pypy.org/), Windows or macOS Silicon, socketify will support Http3 and a lot more features. 

And yes, we can be faster than japronto when all our features and goals are achieved, and we are probably faster than any current maintained solution out there.

## :grey_question: uvloop
We don't use uvloop, because uvloop don't support Windows and PyPy3 at this moment, this can change in the future, but right now we want to implement our own libuv + asyncio solution, and a lot more.

## :dizzy: CFFI vs Cython vs HPy
Cython performs really well on Python3 but really bad on PyPy3, CFFI are choosen for better support PyPy3 until we got our hands on an stable [`HPy`](https://hpyproject.org/) integration.

## :bookmark_tabs: Documentation
See the full docs in [docs.socketify.dev](https://docs.socketify.dev) or in [/docs/README.md](docs/README.md)
