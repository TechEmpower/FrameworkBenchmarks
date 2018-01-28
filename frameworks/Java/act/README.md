# ActFramework Benchmarking Test

This is the ActFramework portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

This is using [ActFramework R1.6.4](http://actframework.org) to build an application.

## Local benchmark result

### Plain text and JSON

| Test | Throughput |
| ---- | ---------: |
| plaintext | 1188983.73  |
| json | 254222.40 |

### MySQL - Ebean

| Test | Throughput |
| ---- | ----------: |
| db | 18160.19 |
| multiple queries (20) | 1918.49 |
| updates (20) | 568.46 |
| fortunes | 18848.39 |

### Postgresql- Ebean

| Test | Throughput |
| ---- | ---------: |
| db | 32303.52 |
| multiple queries (20) | 3268.78 |
| updates (20) | 865 |
| fortunes | 27560 |

### MySQL - JPA/Hibernate

| Test | Throughput |
| ---- | ----------: |
| db | 39588.35 |
| multiple queries (20) | 2518.09 |
| updates (20) | 929.35 |
| fortunes | 33314.78 |

### Postgresql- JPA/Hibernate

| Test | Throughput |
| ---- | ---------: |
| db | 41079.85 |
| multiple queries (20) | 3030.14 |
| updates (20) | 21.59 |
| fortunes | 32490.22 |

### MySQL - JPA/EclipseLink

| Test | Throughput |
| ---- | ----------: |
| db | 98415.07 |
| multiple queries (20) | 44483.81 |
| updates (20) | 2213.31 |
| fortunes | 37590.55 |

### Postgresql- JPA/EclipseLink

| Test | Throughput |
| ---- | ---------: |
| db | 103347.02 |
| multiple queries (20) | 49868.77 |
| updates (20) | 4167.03 |
| fortunes | 34565.04 |

### Mongodb

| Test | Throughput |
| ---- | ---------: |
| db | 33595.57 |
| multiple queries (20) | 2197.53 |
| updates (20) | 1039.60 |
| fortunes | 24301.25 |
