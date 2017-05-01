#include <stdlib.h>
#include <octane.h>
#include "connection.h"

connection* create_connection()
{
    connection* conn = calloc(1, sizeof(connection));
    conn->bytes_remaining = 0;
    conn->request_length = 0;
    conn->keep_alive = true;
    return conn;
}

void free_connection(connection* conn) {
    free(conn);
}
