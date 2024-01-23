# Yii2 Benchmarking Test

This is the Yii2 portion of a [benchmarking test suite](../../../) comparing a variety of web development platforms.

- The [yii2-raw controller](app/controllers/RawController.php) sends raw SQL queries with the Yii DAO.
- The [yii2 controller](app/controllers/SiteController.php) uses the full ORM embedded with Yii2.

This is a realistic implementation, which keeps an ordinary structure and avoids frameworks shortcuts.

## Infrastructure Software

* [Yii2](http://yiiframework.com/)
* [PHP](http://www.php.net/)
* [nginx](http://nginx.org/)
* [MySQL](https://dev.mysql.com/)

## Test URLs

See [benchmark_config.json](benchmark_config.json) for mapping tests to URLs.
