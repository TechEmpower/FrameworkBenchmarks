package database

import com.huanshankeji.exposedvertxsqlclient.ConnectionConfig

val connectionConfig = ConnectionConfig.Socket(
    host = "tfb-database",
    user = "benchmarkdbuser",
    password = "benchmarkdbpass",
    database = "hello_world"
)
