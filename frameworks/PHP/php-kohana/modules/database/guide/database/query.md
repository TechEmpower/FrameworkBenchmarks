# Making Queries

There are two different ways to make queries. The simplest way to make a query is to use [Database_Query], via [DB::query], to manually create queries. These queries are called [parameterized statements](query/parameterized) and allow you to set query parameters which are automatically escaped. The second way to make a query is by building the query using method calls. This is done using the [query builder](query/builder).

[!!] All queries are run using the `execute` method, which accepts a [Database] object or instance name. See [Database_Query::execute] for more information.