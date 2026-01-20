# Codeigniter Benchmarking Test

This is the Codeigniter portion of a [benchmarking test suite](../../../) comparing a variety of web development platforms.

### Plaintext and JSON Encoding Test

Implemented as functions right in [app/Config/Routes.php](app/Config/Routes.php).

### Data-Store/Database Mapping Test

Implemented in two variants:
- ORM based in file [app/Controllers/Full.php](app/Controllers/Full.php)
- Plain SQL based in file [app/Controllers/Raw.php](app/Controllers/Raw.php)

## Test URLs

See [benchmark_config.json](benchmark_config.json) for mapping tests to URLs.
