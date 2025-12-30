# [ElysiaJS](https://elysiajs.com/) - Fast, and friendly Bun web framework

<img src="https://elysiajs.com/assets/feature-sheet.webp" alt="Elysia Feature Sheet" style="max-width: 100%;margin: 0 auto;">

## Introduction
ElysiaJS is a fast, and friendly [Bun](https://bun.sh) web framework.

> <small>Pronounce as "eh-LIHZ-iy-ah"・ エリシア ・ เอลิเซีย</small>

Building on top of 3 philosophies:
- Performance
    - You shall not worry about the underlying performance
- Simplicity
    - Simple building blocks to create an abstraction, not repeating yourself
- Flexibility
    - You shall be able to customize most of the library to fit your need

Designed with TypeScript in mind, you don't need to understand TypeScript to take advantage of Elysia. The library understands what you want and automatically infers the type from your code.

> [__Elysia Docs__](https://elysiajs.com/introduction.html)

## Important Libraries

The tests were run with:

- [elysia](https://github.com/elysiajs/elysia)
- [postgres](https://github.com/porsager/postgres)

## Database
There are **no database endpoints** or drivers attached by default.

To initialize the application with one of these, run any _one_ of the following commands:

```sh
$ DATABASE=postgres bun start
```

## Test URLs
### JSON

http://localhost:8080/json

### PLAINTEXT

http://localhost:8080/plaintext

### DB

http://localhost:8080/db

### QUERY

http://localhost:8080/queries?queries=10

### UPDATE

http://localhost:8080/updates?queries=10

### Fortune

http://localhost:8080/fortunes
