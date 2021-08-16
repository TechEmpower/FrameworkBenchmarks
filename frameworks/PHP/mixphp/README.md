> OpenMix 出品：https://openmix.org

<p align="center">
    <br>
    <br>
    <img src="https://openmix.org/static/image/logo_php.png" width="120" alt="MixPHP">
    <br>
    <br>
</p>

<h1 align="center">Mix PHP</h1>

中文 | [English](https://github.com/mix-php/mix/blob/master/README_EN.md)

MixPHP 是一个 PHP 命令行模式开发框架；基于 `Vega` 驱动的 HTTP 可以同时支持 Swoole、WorkerMan、FPM、CLI-Server 生态，并且可以无缝切换；`V3` 是一个高度解耦的版本，整体代码基于多个独立的模块构建，即便用户不使用我们的脚手架，也可以使用这些独立模块，并且全部模块都支持原生开发。例如：你可以只使用 mix/vega 来搭配 laravel orm 使用；可以在任意环境中使用 mix/database 和 mix/redis；可以使用 mix/grpc 原生代码编写 gRPC；所有的模块你可以像搭积木一样随意组合。

## 独立模块

核心模块全部可独立使用，并且都支持原生代码开发。

- [mix/vega](https://github.com/mix-php/vega) PHP 编写的 CLI 模式 HTTP 网络框架，支持 Swoole、WorkerMan，与 Go 生态的 gin 定位一致
- [mix/database](https://github.com/mix-php/database) 可在各种环境中使用的轻量数据库，支持 FPM、CLI、Swoole、WorkerMan，可选的连接池 (协程)
- [mix/redis](https://github.com/mix-php/redis) 可在各种环境中使用的 PHP Redis，支持 FPM、CLI、Swoole、WorkerMan，可选的连接池 (协程)
- [mix/redis-subscriber](https://github.com/mix-php/redis-subscriber) 基于 Swoole 协程的 Redis 原生协议订阅库
- [mix/grpc](https://github.com/mix-php/grpc) 基于 Swoole 协程的 PHP gRPC 库，包含 protoc 代码生成器、服务器、客户端
- [mix/websocket](https://github.com/mix-php/websocket) 基于 Swoole 协程的 PHP WebSocket 服务器与客户端
- [mix/cli](https://github.com/mix-php/cli) PHP 命令行交互指挥官
- [mix/worker-pool](https://github.com/mix-php/worker-pool) 基于 Swoole 的协程池、工作池库
- [mix/validator](https://github.com/mix-php/validator) 基于 PSR-7 的验证库
- [mix/event](https://github.com/mix-php/event) 基于 PSR-14 标准的事件调度库

## 快速开始

提供了现成的脚手架，快速创建项目，立即产出。

- [编写一个 CLI 程序](https://github.com/mix-php/cli-skeleton#readme)

```
composer create-project --prefer-dist mix/cli-skeleton cli
```

- [编写一个 API 接口](https://github.com/mix-php/api-skeleton#readme)

```
composer create-project --prefer-dist mix/api-skeleton api
```

- [编写一个 Web 页面](https://github.com/mix-php/web-skeleton#readme)

```
composer create-project --prefer-dist mix/web-skeleton web
```

- [编写一个 WebSocket 服务](https://github.com/mix-php/websocket-skeleton#readme)

```
composer create-project --prefer-dist mix/websocket-skeleton websocket
```

- [编写一个 gRPC 接口](https://github.com/mix-php/grpc-skeleton#readme)

```
composer create-project --prefer-dist mix/grpc-skeleton grpc
```

## 技术交流

知乎：https://www.zhihu.com/people/onanying    
官方QQ群：[284806582](https://shang.qq.com/wpa/qunwpa?idkey=b3a8618d3977cda4fed2363a666b081a31d89e3d31ab164497f53b72cf49968a), [825122875](http://shang.qq.com/wpa/qunwpa?idkey=d2908b0c7095fc7ec63a2391fa4b39a8c5cb16952f6cfc3f2ce4c9726edeaf20) 敲门暗号：phper

## Golang 框架

OpenMix 同时还有 Golang 生态的框架

- https://github.com/mix-go/mix

## 旧版文档

- `V1` https://www.kancloud.cn/onanying/mixphp1/content
- `V2` https://www.kancloud.cn/onanying/mixphp2/content
- `V2.1` https://www.kancloud.cn/onanying/mixphp2-1/content
- `V2.2` https://www.kancloud.cn/onanying/mixphp2-2/content

## License

Apache License Version 2.0, http://www.apache.org/licenses/
