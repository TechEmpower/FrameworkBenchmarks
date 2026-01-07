# Tad.x (tadx) Benchmarking Test

## Project Overview
Tad.x is a Java web framework project for benchmarking, part of the [FrameworkBenchmarks](https://github.com/TechEmpower/FrameworkBenchmarks) project, designed to test the performance of different web frameworks.

## Technology Stack
- **Java 21**: The primary development language
- **Spring Boot**: The main framework currently used (previously used Vert.x, now commented out)
- **Vert.x**: For asynchronous event handling and database operations
- **PostgreSQL**: Database used for benchmarking
- **Thymeleaf & FreeMarker**: Template engines for the Fortunes test
- **Gradle**: Project build tool

## Project Structure
```yaml
tadx/ 
├── src/main/java/io/tadx/benchmark/ 
│ ├── Application.java # Project entry point 
│ ├── controller/ # Controller classes 
│ ├── entity/ # Database entity classes 
│ └── route_mapper/ # Route mapping implementations 
├── src/main/resources/ # Resource files 
│ ├── application.yaml # Configuration file 
│ └── templates/ # Template files 
├── build.gradle # Gradle build configuration 
├── settings.gradle # Gradle settings 
└── tadx.dockerfile # Docker deployment configuration
```


plainText

## Core Features
The project implements the following benchmark test types:

| Test Type | Route | Implementation Class | Description |
|---------|------|-------|----------|
| JSON | /json | JsonRouteMapper.java | Returns a simple JSON response |
| PLAINTEXT | /plaintext | PlainTextRouteMapper.java | Returns a simple text response |
| DB | /db | DbRouteMapper_Postgresql.java | Single database query |
| QUERY | /query | QueriesRouteMapper1_Postgresql.java | Multiple database queries |
| CACHED QUERY | /cached_query | CachedQueriesMapper3.java | Caches query results |
| UPDATE | /update | UpdateMapper.java | Database update operations |
| FORTUNES | /fortunes | FortunesRouteMapper1.java | Template rendering test |

## Implementation Features

### Routing Mechanism
- Uses custom `@RouteMapping` annotation to define routes
- Each test type corresponds to a `RouteMapper` interface implementation
- Route handling directly manipulates Vert.x response objects to reduce overhead

### Database Operations
- Uses Vert.x PostgreSQL client for asynchronous database operations
- Configures database connection pool with maximum 2000 connections
- Supports prepared statements caching for improved performance
- Defines `World` and `Fortune` entity classes mapping to database tables

### Performance Optimization
- Directly sets HTTP response headers and status codes to reduce framework overhead
- Uses precompiled statements and connection pooling to improve database performance
- Caches commonly used response headers and date strings

## Running Methods
1. **Direct Run**: Start the Spring Boot application through the main method in `Application.java`
2. **Build and Run**: Build JAR file using Gradle and run it
3. **Docker Deployment**: Build image using the provided tadx.dockerfile and run it

## Configuration Files
- `application.yaml`: Spring Boot application configuration
- `benchmark_config.json`: Benchmark configuration

## Test Type Implementation Source Code

* [JSON](src/main/java/io/tadx/benchmark/route_mapper/JsonRouteMapper.java)
* [PLAINTEXT](src/main/java/io/tadx/benchmark/route_mapper/PlainTextRouteMapper.java)
* [DB](src/main/java/io/tadx/benchmark/route_mapper/DbRouteMapper_Postgresql.java)
* [QUERY](src/main/java/io/tadx/benchmark/route_mapper/QueriesRouteMapper1_Postgresql.java)
* [CACHED QUERY](src/main/java/io/tadx/benchmark/route_mapper/CachedQueriesMapper3.java)
* [UPDATE](src/main/java/io/tadx/benchmark/route_mapper/UpdateMapper.java)
* [FORTUNES](src/main/java/io/tadx/benchmark/route_mapper/FortunesRouteMapper1.java)


## Test URLs
### JSON

http://localhost:8000/json

### PLAINTEXT

http://localhost:8000/plaintext

### DB

http://localhost:8000/db

### QUERY

http://localhost:8000/query?queries=

### CACHED QUERY

http://localhost:8000/cached_query?queries=

### UPDATE

http://localhost:8000/update?queries=

### FORTUNES

http://localhost:8000/fortunes