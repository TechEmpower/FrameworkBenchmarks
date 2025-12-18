rootProject.name = "vertx-web-kotlinx-benchmark"

include(
    "common",

    "without-db:default",

    "with-db:common",
    "with-db:default",
    "with-db:exposed-r2dbc",
    "with-db:exposed-vertx-sql-client"
)
