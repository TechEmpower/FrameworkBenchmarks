# ActFramework Benchmarking Test

This is the ActFramework portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

This is using [ActFramework R1.6.4](http://actframework.org) to build an application.

## Local benchmark result

### Plain text and JSON

| Test | Throughput |
| ---- | ---------: |
| plaintext | 1188983.73  |
| json | 254222.40 |

### MySQL

| Test | Throughput |
| ---- | ----------: |
| db | 18160.19 |
| multiple queries (20) | 1918.49 |
| updates (20) | 568.46 |
| fortunes | 18848.39 |

### Postgresql

| Test | Throughput |
| ---- | ---------: |
| db | 32303.52 |
| multiple queries (20) | 3268.78 |
| updates (20) | 865 |
| fortunes | 27560 |


### Mongodb

| Test | Throughput |
| ---- | ---------: |
| db | 33595.57 |
| multiple queries (20) | 2197.53 |
| updates (20) | 1039.60 |
| fortunes | 24301.25 |
