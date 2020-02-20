# nestjs-fastify Benchmarking Test

### Test Type Implementation Source Code

- [Init](src/main.ts)
- [Routes](src/app.controller.ts)
- [Logic](src/app.service.ts)

## Important Libraries

The tests were run with:

- [NestJS](https://nestjs.com/)
- [NestFastify](https://docs.nestjs.com/techniques/performance)
- [Fastify](https://www.fastify.io/)
- [TypeORM](https://typeorm.io/)
- [Caching](https://docs.nestjs.com/techniques/caching)
- [MVC](https://docs.nestjs.com/techniques/mvc#fastify)

## Test URLs

### JSON

http://localhost:3000/json

### PLAINTEXT

http://localhost:3000/plaintext

### DB

http://localhost:3000/db

### QUERY

http://localhost:3000/queries?queries=

### CACHED QUERY

http://localhost:3000/cached-worlds?count=

### UPDATE

http://localhost:3000/updates?queries=

### FORTUNES

http://localhost:3000/fortunes
