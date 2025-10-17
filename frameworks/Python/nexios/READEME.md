
## `NEXIOS` 

<div align="left">

<a href="https://git.io/typing-svg"><img src="https://readme-typing-svg.demolab.com?font=Fira+Code&pause=1000&color=4CAF50&center=true&width=435&lines=Nexios+ASGI+Framework;Fast%2C+Simple%2C+Flexible" alt="Typing SVG" /></a>

<p align="center">
  <a href="">
    <img alt=Support height="350" src="https://nexios-docs.netlify.app/icon.svg"> 
    </p>
    <h1 align="center">Nexios 2.4.x<br></h1>
    
   </a>
</p>

<!-- Badges Section -->
<p align="center">
  <img src="https://img.shields.io/badge/Python-3.9+-blue?logo=python" alt="Python Version">
  <img src="https://img.shields.io/badge/Downloads-10k/month-brightgreen" alt="Downloads">
  <img src="https://img.shields.io/badge/Contributions-Welcome-orange" alt="Contributions">
  <img src="https://img.shields.io/badge/Active Development-Yes-success" alt="Active Development">
</p>

<p align="center">
<a href="https://github.com/nexios-labs/Nexios?tab=followers"><img title="Followers" src="https://img.shields.io/github/followers/nexios-labs?label=Followers&style=social"></a>
<a href="https://github.com/nexios-labs/Nexios/stargazers/"><img title="Stars" src="https://img.shields.io/github/stars/nexios-labs/Nexios?&style=social"></a>
<a href="https://github.com/nexios-labs/Nexios/network/members"><img title="Fork" src="https://img.shields.io/github/forks/nexios-labs/Nexios?style=social"></a>
<a href="https://github.com/nexios-labs/Nexios/watchers"><img title="Watching" src="https://img.shields.io/github/watchers/nexios-labs/Nexios?label=Watching&style=social"></a>


</br>

<h2 align="center"> Star the repo if u like it🌟
</h2>

Nexios is a high-performance Python web framework. Designed for speed, flexibility, and simplicity, Nexios delivers exceptional performance through its native Rust engine while maintaining the simplicity and elegance of Python. It supports RESTful APIs, authentication, and integrates easily with any ORM. Built for modern web development, Nexios allows developers to quickly spin up scalable, modular apps with minimal boilerplate—ideal for startups, rapid prototyping, and custom backend solutions. Think Django's capability with Rust-powered speed.

---


## `Installation` 📦

To install **Nexios**, you can use several methods depending on your environment and preferred package manager. Below are the instructions for different package managers:

### 1. **From `pip`** (Standard Python Package Manager)

```bash
pip install nexios
```


## Features ✨

- [x] **Routing**
- [x] **Automatic OpenAPI Documentation**
- [x] **Session Management**
- [x] **File Router**  
- [x] **Authentication (Limited)**
- [x] **Event Listener for Signals** 
- [x] **Middleware Support**
- [x] **Express-like Functionality**
- [x] **JWT Authentication**
- [x] **Pydantic Support**
- [x] **Dependency Injection**
- [x] **In-built Support for CORS**  
- [x] **Custom Decorators**
- [x] **WebSocket Support**  
- [x] **Custom Error Handling**
- [x] **Pagination**
- [x] **HTTP/2 Support**  
- [x] **High-Performance Async Processing**

### Upcoming Features

- [ ] **Inbuilt Database ORM Integration**
- [ ] **Asynchronous Task Queue**
- [ ] **Rate Limiting**
- [ ] **API Throttling**

### Basic Example

```py
from nexios import NexiosApp
from nexios.http import Request, Response

app = NexiosApp()

@app.get("/")
async def basic(request: Request, response: Response):
    return {"message": "Hello, world!"}
    # return response.json({"message":"Hello, world!"}) ## This will work for more control


```

### Another Basic Example

```py
from nexios import NexiosApp, Depend
from nexios.http import Request, Response

app = NexiosApp()

async def get_user():
    return {"name": "John Doe"}


@app.get("/users")
async def get_user(request: Request, response: Response, user: Depend(get_user)):
   
    return {"user": user}
```

Visit http://localhost:4000/docs to view the Swagger API documentation.  



### Testimonies

> "Adopting Nexios at our startup has been a practical and effective choice. In a fast-moving development environment, we needed something lightweight and efficient — Nexios met that need.
>
> Its clean architecture and compatibility with different ORMs helped our team work more efficiently and keep things maintainable. One of its strengths is how straightforward it is — minimal overhead, just the tools we need to build and scale our backend services.
>
> Credit to Dunamis for introducing Nexios to the team. It’s now a steady part of our stack, and it’s serving us well.
> — Joseph Mmadubuike , Chief Technology Officer buzzbuntu.com

## See the full docs

👉 <a href="https://nexios-docs.netlify.app">https://nexios-docs.netlify.app</a>

## Contributors:
<a href="https://github.com/nexios-labs/nexios/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=nexios-labs/nexios" />
</a>




