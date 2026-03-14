package database

import com.huanshankeji.exposedvertxsqlclient.ConnectionConfig

val connectionConfig = ConnectionConfig.Socket(
    host = HOST,
    user = USER,
    password = PASSWORD,
    database = DATABASE
)
