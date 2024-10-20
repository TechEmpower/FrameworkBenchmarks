English | [中文](./README-CN.md)

[![Build Status](https://travis-ci.org/hyperf-cloud/hyperf.svg?branch=master)](https://travis-ci.org/hyperf-cloud/hyperf)
[![Php Version](https://img.shields.io/badge/php-%3E=7.2-brightgreen.svg?maxAge=2592000)](https://secure.php.net/)
[![Swoole Version](https://img.shields.io/badge/swoole-%3E=4.3.3-brightgreen.svg?maxAge=2592000)](https://github.com/swoole/swoole-src)
[![Hyperf License](https://img.shields.io/github/license/hyperf-cloud/hyperf.svg?maxAge=2592000)](https://github.com/hyperf-cloud/hyperf/blob/master/LICENSE.md)

# Introduction

Hyperf is a high-performance, highly flexible PHP CLI framework based on `Swoole 4.3+`. It has a built-in coroutine server with a large number of commonly used components. It provides ultra-high and better performance than the traditional PHP-FPM-based framework and also maintains extremely flexible scalability at the same time. Standard components are implemented in the latest PSR standards, and a powerful dependency injection design ensures that most components or classes within the framework are replaceable.

In addition to providing `MySQL coroutine client` and `Redis coroutine client`, common coroutine clients, the Hyperf component libraries are also prepared for the coroutine version of `Eloquent ORM`, `GRPC server and client`, `Zipkin (OpenTracing) client`, `Guzzle HTTP client`, `Elasticsearch client`, `Consul client`, `ETCD client`, `AMQP component`, `Apollo configuration center`, `Token bucket algorithm-based limiter`, and `Universal connection pool`, etc. Therefore, the trouble of implementing the corresponding coroutine version client by yourself can be avoided. Hyperf also provides convenient functions such as `Dependency injection`, `Annotation`, `AOP (aspect-oriented programming)`, `Middleware`, `Custom Processes`, `Event Manager`, `Simply Redis message queue`, and `Full-featured RabbitMQ message queue` to meet a wide range of technical and business scenarios.

# Original intention

Although there are many new PHP frameworks have been appeared, but we still has not seen a perfect framework which has the coexistence of elegant design and ultra-high performance, nor would we find a framework that really paves the way for PHP microservices. For the original intention of Hyperf and its team members, we will continue to invest to it, and you are welcome to join us to participate in open source construction.

# Design concept

`Hyperspeed + Flexibility = Hyperf`, from the framework name we have been used `hyperfspeed (ultra-high performance)` and `flexibility` as the gene of Hyperf.

For ultra-high performance, Hyperf based on the Swoole coroutine, it providered an amazing performance, Hyperf team also makes a lots of code optimizations on the framework design to ensure ultra-high performance.   

For flexibility, based on the powerful dependency injection component  of Hyperf, all components are based on [PSR](https://www.php-fig.org/psr) and the contracts that defined by Hyperf, so that most of the components or classes within the framework are replaceable and re-useable.   

Based on the above characteristics, Hyperf has a lots of possibilities, such as implementing Web servers, gateway servers, distributed middleware software, microservices architecture, game servers, and Internet of Things (IoT).

# Documentation

[https://doc.hyperf.io/](https://doc.hyperf.io/)
