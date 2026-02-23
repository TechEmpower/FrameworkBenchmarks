# Tad.x (tadx) 基准测试

## 项目概述
Tad.x 是一个用于基准测试的 Java Web 框架项目，是 [FrameworkBenchmarks](https://github.com/TechEmpower/FrameworkBenchmarks) 的一部分，主要用于测试不同 Web 框架的性能表现。

## 技术栈
- **Java 21**：项目的主要开发语言
- **Spring Boot**：当前使用的主要框架（从代码中可以看到之前可能使用过 Vert.x，现已注释）
- **Vert.x**：用于异步事件处理和数据库操作
- **PostgreSQL**：基准测试使用的数据库
- **Thymeleaf & FreeMarker**：模板引擎，用于Fortunes测试
- **Gradle**：项目构建工具

## 项目结构
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

## 核心功能
项目实现了以下基准测试类型：

| 测试类型 | 路由 | 实现类 | 功能描述 |
|---------|------|-------|----------|
| JSON | /json | JsonRouteMapper.java | 返回简单JSON响应 |
| 文本 | /plaintext | PlainTextRouteMapper.java | 返回简单文本响应 |
| 数据库 | /db | DbRouteMapper_Postgresql.java | 单条数据库查询 |
| 多查询 | /query | QueriesRouteMapper1_Postgresql.java | 多条数据库查询 |
| 缓存查询 | /cached_query | CachedQueriesMapper3.java | 缓存查询结果 |
| 更新 | /update | UpdateMapper.java | 数据库更新操作 |
| 幸运饼干 | /fortunes | FortunesRouteMapper1.java | 模板渲染测试 |

## 实现特点

### 路由机制
- 使用自定义的 `@RouteMapping` 注解定义路由
- 每个测试类型对应一个 `RouteMapper` 接口实现
- 路由处理直接操作 Vert.x 的响应对象，减少中间层开销

### 数据库操作
- 使用 Vert.x 的 PostgreSQL 客户端进行异步数据库操作
- 配置了数据库连接池，最大连接数为 2000
- 支持 prepared statements 缓存，提高性能
- 定义了 `World` 和 `Fortune` 两个实体类映射数据库表

### 性能优化
- 直接设置 HTTP 响应头和状态码，减少框架开销
- 使用预编译语句和连接池提高数据库性能
- 缓存常用的响应头和日期字符串

## 运行方式
1. **直接运行**：通过 `Application.java` 的 main 方法启动 Spring Boot 应用
2. **构建运行**：使用 Gradle 构建 JAR 文件后运行
3. **Docker部署**：使用提供的 tadx.dockerfile 构建镜像并运行

## 配置文件
- `application.yaml`：Spring Boot 应用配置
- `benchmark_config.json`：基准测试配置

## 测试类型实现源代码

* [JSON](src/main/java/io/tadx/benchmark/route_mapper/JsonRouteMapper.java)
* [PLAINTEXT](src/main/java/io/tadx/benchmark/route_mapper/PlainTextRouteMapper.java)
* [DB](src/main/java/io/tadx/benchmark/route_mapper/DbRouteMapper_Postgresql.java)
* [QUERY](src/main/java/io/tadx/benchmark/route_mapper/QueriesRouteMapper1_Postgresql.java)
* [CACHED QUERY](src/main/java/io/tadx/benchmark/route_mapper/CachedQueriesMapper3.java)
* [UPDATE](src/main/java/io/tadx/benchmark/route_mapper/UpdateMapper.java)
* [FORTUNES](src/main/java/io/tadx/benchmark/route_mapper/FortunesRouteMapper1.java)


## 测试URLs
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
