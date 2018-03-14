FROM tfb/aiohttpweb-base:latest

WORKDIR /aiohttp.web

CMD api_hour -ac hello:Container
