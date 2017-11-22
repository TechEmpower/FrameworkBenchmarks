# ActFramework Benchmarking Test

This is the ActFramework portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

This is using [ActFramework R1.5.1](http://actframework.org) to build an application.

## Local benchmark result

### Plain text and JSON

| Test | Throughput |
| ---- | ---------: |
| plaintext | 671911.83  |
| json | 167184.71 |

### MySQL

| Test | Throughput |
| ---- | ----------: |
| db | 15726.85 |
| multiple queies (20) | 1634.25 |
| updates (20) | 112.40 |
| fortunes | 13340.19 |

### Postgresql

| Test | Throughput |
| ---- | ---------: |
| db | 19178.11 |
| multiple queies (20) | 2108/46 |
| updates (20) | 110.91 |
| fortunes | 17008.39 |


### Mongodb

| Test | Throughput |
| ---- | ---------: |
| db | 19156.71 |
| multiple queies (20) | 1821.24 |
| updates (20) | 849.83 |
| fortunes | 17603.13 |
