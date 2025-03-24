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
