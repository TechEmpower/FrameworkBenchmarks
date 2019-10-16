<p align="center">
    <a href="https://www.imiphp.com" target="_blank">
        <img src="https://raw.githubusercontent.com/Yurunsoft/IMI/dev/res/logo.png" alt="imi" />
    </a>
</p>

[![Latest Version](https://img.shields.io/packagist/v/yurunsoft/imi.svg)](https://packagist.org/packages/yurunsoft/imi)
[![Travis](https://img.shields.io/travis/Yurunsoft/IMI.svg)](https://travis-ci.org/Yurunsoft/IMI)
[![Php Version](https://img.shields.io/badge/php-%3E=7.1-brightgreen.svg)](https://secure.php.net/)
[![Swoole Version](https://img.shields.io/badge/swoole-%3E=4.3.0-brightgreen.svg)](https://github.com/swoole/swoole-src)
[![imi Doc](https://img.shields.io/badge/docs-passing-green.svg)](https://doc.imiphp.com)
[![imi License](https://img.shields.io/badge/license-MulanPSL%201.0-brightgreen.svg)](https://github.com/Yurunsoft/imi/blob/master/LICENSE)

## 介绍

imi 是基于 PHP Swoole 的高性能协程应用开发框架，它支持 HttpApi、WebSocket、TCP、UDP 服务的开发。

在 Swoole 的加持下，相比 php-fpm 请求响应能力，I/O密集型场景处理能力，有着本质上的提升。

imi 框架拥有丰富的功能组件，可以广泛应用于互联网、移动通信、企业软件、云计算、网络游戏、物联网（IOT）、车联网、智能家居等领域。可以使企业 IT 研发团队的效率大大提升，更加专注于开发创新产品。

imi 框架交流群：17916227 [![点击加群](https://pub.idqqimg.com/wpa/images/group.png "点击加群")](https://jq.qq.com/?_wv=1027&k=5wXf4Zq)

### 核心组件

* HttpApi、WebSocket、TCP、UDP 服务器
* MySQL 连接池 (主从+负载均衡)
* Redis 连接池 (主从+负载均衡)
* 超好用的 ORM (Db、Redis、Tree)
* 毫秒级热更新
* AOP
* Bean 容器
* 缓存 (Cache)
* 配置读写 (Config)
* 枚举 (Enum)
* 事件 (Event)
* 门面 (Facade)
* 验证器 (Validate)
* 锁 (Lock)
* 日志 (Log)
* 异步任务 (Task)

### 扩展组件

* [RPC](https://github.com/imiphp/imi-rpc)
* [Hprose](https://github.com/imiphp/imi-hprose)
* [权限控制](https://github.com/imiphp/imi-access-control)
* [Smarty 模版引擎](https://github.com/imiphp/imi-smarty)
* [限流](https://github.com/imiphp/imi-rate-limit)
* [跨进程变量共享](https://github.com/imiphp/imi-shared-memory)
* [Swoole Tracker](https://github.com/imiphp/imi-swoole-tracker)

## 开始使用

创建 Http Server 项目：`composer create-project imiphp/project-http`

创建 WebSocket Server 项目：`composer create-project imiphp/project-websocket`

创建 TCP Server 项目：`composer create-project imiphp/project-tcp`

创建 UDP Server 项目：`composer create-project imiphp/project-udp`

[完全开发手册](https://doc.imiphp.com)

## 运行环境

- Linux 系统 (Swoole 不支持在 Windows 上运行)
- [PHP](https://php.net/) >= 7.1
- [Composer](https://getcomposer.org/)
- [Swoole](https://www.swoole.com/) >= 4.3.0
- Redis、PDO 扩展

## 版权信息

imi 遵循 木兰宽松许可证(Mulan PSL v1) 开源协议发布，并提供免费使用。

## 鸣谢

感谢以下开源项目 (按字母顺序排列) 为 imi 提供强力支持！

- [doctrine/annotations](https://github.com/doctrine/annotations) (PHP 注解处理类库)
- [PHP](https://php.net/) (没有 PHP 就没有 imi)
- [Swoole](https://www.swoole.com/) (没有 Swoole 就没有 imi)

## 贡献者

<a href="https://github.com/Yurunsoft/IMI/graphs/contributors"><img src="https://opencollective.com/IMI/contributors.svg?width=890&button=false" /></a>

你想出现在贡献者列表中吗？

你可以做的事（包括但不限于以下）：

* 纠正拼写、错别字
* 完善注释
* bug修复
* 功能开发
* 文档编写（<https://github.com/Yurunsoft/imidoc>）
* 教程、博客分享

> 最新代码以 `dev` 分支为准，提交 `PR` 也请合并至 `dev` 分支！

提交 `Pull Request` 到本仓库，你就有机会成为 imi 的作者之一！

## 关于测试脚本

### 环境要求

Redis、MySQL

### 首次运行测试

* 创建 `db_imi_test` 数据库，将 `tests/db/db.sql` 导入到数据库

* 配置系统环境变量，如果默认值跟你的一样就无需配置了

名称 | 描述 | 默认值
-|-|-
MYSQL_SERVER_HOST | MySQL 主机名 | 127.0.0.1 |
MYSQL_SERVER_PORT | MySQL 端口 | 3306 |
MYSQL_SERVER_USERNAME | MySQL 用户名 | root |
MYSQL_SERVER_PASSWORD | MySQL 密码 | root |
REDIS_SERVER_HOST | Redis 主机名 | 127.0.0.1 |
REDIS_SERVER_PORT | Redis 端口 | 6379 |
REDIS_SERVER_PASSWORD | Redis 密码 |  |
REDIS_CACHE_DB | Redis 缓存用的 `db`，该 `db` 会被清空数据，请慎重设置 | 1 |

配置命令：`export NAME=VALUE`

* 首次运行测试脚本：`composer install-test`

* 首次之后再运行测试的命令：`composer test`

## 捐赠

<img src="https://raw.githubusercontent.com/Yurunsoft/IMI/dev/res/pay.png"/>

开源不求盈利，多少都是心意，生活不易，随缘随缘……
