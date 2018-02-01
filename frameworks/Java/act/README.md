# ActFramework Benchmarking Test

This is the ActFramework portion of a [benchmarking test suite](../) comparing a variety of web development platforms.

This is using [ActFramework 1.7.0-teb-SNAPSHOT](http://actframework.org) to build an application.

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
| db | 20134.12 |
| multiple queries (20) | 1187.38 |
| updates (20) | 560.91 |
| fortunes | 20334.32 |

### Postgresql- JPA/EclipseLink

| Test | Throughput |
| ---- | ---------: |
| db | 31906.95 |
| multiple queries (20) | 1935 |
| updates (20) | 650.98 |
| fortunes | 19764.26 |

### Mongodb

| Test | Throughput |
| ---- | ---------: |
| db | 33595.57 |
| multiple queries (20) | 2197.53 |
| updates (20) | 1039.60 |
| fortunes | 24301.25 |

## SQL database test results comparing

### DB

| Technology | MySQL | Postgresql |
| ---------- | ----: | ---------: |
| Ebean | 18160.19 | 32303.52 |
| JPA/Hibernate | 39588.35 | 41079.85 |
| JPA/EclipseLink | 98415.07 | 103347.02 |

### Multiple Queries

| Technology | MySQL | Postgresql |
| ---------- | ----: | ---------: |
| Ebean | 1918.49 | 3268.78 |
| JPA/Hibernate | 2518.09 | 3030.14 |
| JPA/EclipseLink | 44483.81 | 49868.77  |


### Updates

| Technology | MySQL | Postgresql |
| ---------- | ----: | ---------: |
| Ebean | 568.46 | 865 |
| JPA/Hibernate | 929.35 | 21.59 |
| JPA/EclipseLink | 2213.31 | 4167.03 |

### Fortunes

| Technology | MySQL | Postgresql |
| ---------- | ----: | ---------: |
| Ebean | 18848.39 | 27560 |
| JPA/Hibernate | 33314.78 | 32490.22 |
| JPA/EclipseLink | 37590.55 | 34565.04 |

