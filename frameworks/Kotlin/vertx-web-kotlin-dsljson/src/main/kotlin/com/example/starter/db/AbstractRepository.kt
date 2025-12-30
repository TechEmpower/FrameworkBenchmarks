package com.example.starter.db

import io.vertx.pgclient.PgConnection

abstract class AbstractRepository<T>(
    protected val conn: PgConnection
)