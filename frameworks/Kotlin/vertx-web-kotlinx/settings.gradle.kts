rootProject.name = "vertx-web-kotlinx-benchmark"

include(
    "common",

    "without-db:default",

    "with-db:common",
    "with-db:default",
    "with-db:r2dbc-common",
    "with-db:r2dbc",
    //"with-db:r2dbc:jasync", // TODO
    "with-db:exposed-common",
    /*
    "with-db:exposed-common:jdbc",
    "with-db:exposed-common:r2dbc",
    */
    "with-db:exposed-r2dbc",
    "with-db:exposed-vertx-sql-client"
)

// renamed explicitly to work around jar name conflicts
project(":with-db:common").name = "with-db-common"
