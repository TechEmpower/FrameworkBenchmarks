<p align="center"><a href="https://hypervel.org" target="_blank"><img src="https://hypervel.org/logo.png" width="400"></a></p>

<p align="center">
<a href="https://github.com/hypervel/hypervel/actions"><img src="https://github.com/hypervel/hypervel/workflows/tests/badge.svg" alt="Build Status"></a>
<a href="https://packagist.org/packages/hypervel/framework"><img src="https://img.shields.io/packagist/dt/hypervel/framework" alt="Total Downloads"></a>
<a href="https://packagist.org/packages/hypervel/hypervel"><img src="https://img.shields.io/packagist/v/hypervel/hypervel" alt="Latest Stable Version"></a>
<a href="https://packagist.org/packages/hypervel/hypervel"><img src="https://img.shields.io/packagist/l/hypervel/hypervel" alt="License"></a>
</p>

## Introduction

**Hypervel** is a Laravel-style PHP framework with native coroutine support for ultra-high performance.

Hypervel ports many core components from Laravel while maintaining familiar usage patterns, making it instantly accessible to Laravel developers. The framework combines the elegant and expressive development experience of Laravel with the powerful performance benefits of coroutine-based programming. If you're a Laravel developer, you'll feel right at home with this framework, requiring minimal learning curve.

This is an ideal choice for building microservices, API gateways, and high-concurrency applications where traditional PHP frameworks often encounter performance constraints.

## Why Hypervel?

While Laravel Octane impressively enhances your Laravel application's performance, it's crucial to understand the nature of modern web applications. In most cases, the majority of latency stems from I/O operations, such as file operations, database queries, and API requests.

However, Laravel doesn't support coroutines - the entire framework is designed for a blocking I/O environment. Applications heavily dependent on I/O operations will still face performance bottlenecks. Consider this scenario:

Imagine building an AI-powered chatbot where each conversation API takes 3-5 seconds to respond. With 10 workers in Laravel Octane receiving 10 concurrent requests, all workers would be blocked until these requests complete.

> You can see [benchmark comparison](https://hypervel.org/docs/introduction.html#benchmark) between Laravel Octane and Hypervel

Even with Laravel Octane's improvements, your application's concurrent request handling capacity remains constrained by I/O operation duration. Hypervel addresses this limitation through coroutines, enabling efficient handling of concurrent I/O operations without blocking workers. This approach significantly enhances performance and concurrency for I/O-intensive applications.

> See [this issue](https://github.com/laravel/octane/issues/765) for more discussions.

## Documentation

[https://hypervel.org/docs](https://hypervel.org/docs)

Hypervel provides comprehensive and user-friendly documentation that allows you to quickly get started. From this documentation, you can learn how to use various components in Hypervel and understand the differences between this framework and Laravel.

> Most of the content in this documentation is referenced from the official Laravel documentation. We appreciate the Laravel community's contributions.

## License

The Hypervel framework is open-sourced software licensed under the [MIT](https://opensource.org/licenses/MIT) license.