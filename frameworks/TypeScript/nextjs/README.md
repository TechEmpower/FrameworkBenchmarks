# Next.js Benchmarking Test

## Test source files and URLs

| Test | Source Code | URL |
| --- | --- | --- |
| [JSON Serialization][] | [`app/json/route.ts`][] | http://localhost:3000/json |
| [Single Database Query][] | [`app/db/route.ts`][] | http://localhost:3000/db |
| [Multiple Database Queries][] | [`app/queries/route.ts`][] | http://localhost:3000/queries?queries= |
| [Fortunes][] | [`app/fortunes/page.tsx`][] | http://localhost:3000/fortunes |
| [Database Updates][] | [`app/updates/route.ts`][] | http://localhost:3000/updates?queries= |
| [Plaintext][] | [`app/plaintext/route.ts`][] | http://localhost:3000/plaintext |
| [Caching][] | [`app/cached-queries/route.ts`][] | http://localhost:3000/cached-queries?queries= |

[JSON Serialization]: https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#json-serialization
[Single Database Query]: https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#single-database-query
[Multiple Database Queries]: https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#multiple-database-queries
[Fortunes]: https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#fortunes
[Database Updates]: https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#database-updates
[Plaintext]: https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#plaintext
[Caching]: https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#caching

[`app/json/route.ts`]: ./app/json/route.ts
[`app/db/route.ts`]: ./app/db/route.ts
[`app/queries/route.ts`]: ./app/queries/route.ts
[`app/fortunes/page.tsx`]: ./app/fortunes/page.tsx
[`app/updates/route.ts`]: ./app/updates/route.ts
[`app/plaintext/route.ts`]: ./app/plaintext/route.ts
[`app/cached-queries/route.ts`]: ./app/cached-queries/route.ts

## TODO

The Fortunes test is currently disabled because the benchmark expects exact HTML output — see [TechEmpower/FrameworkBenchmarks#9505](https://github.com/TechEmpower/FrameworkBenchmarks/pull/9505).  After that issue is resolved, the Fortunes test can be re-enabled by applying the following diff:

```diff
--- a/frameworks/TypeScript/nextjs/benchmark_config.json
+++ b/frameworks/TypeScript/nextjs/benchmark_config.json
@@ -20,7 +20,7 @@
         "json_url": "/json",
         "db_url": "/db",
         "query_url": "/queries?queries=",
-        "TEMPORARILY DISABLED fortune_url": "/fortunes",
+        "fortune_url": "/fortunes",
         "update_url": "/updates?queries=",
         "plaintext_url": "/plaintext",
         "cached_query_url": "/cached-queries?queries="
```
